;+
; $Id: ssg_flatgen.pro,v 1.2 2002/11/08 03:12:09 jpmorgen Exp $

; ssg_flatgen Generate a bestflat frame from all the flat images in a
; given directory

;-
pro ssg_flatgen, indir, outname, showplots=showplots, TV=tv, cr_cutval=cr_cutval, flat_cut=flat_cut, skyflat=skyflat_name
;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(outname) then outname = 'bestflat.fits'
  if NOT keyword_set(cr_cutval) then cr_cutval = 5
  ;; For normalization
  if NOT keyword_set(flat_cut) then flat_cut = 0.75
  if NOT keyword_set(skyflat_name) then skyflat_name = 'skyflat.fits'


  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode=3", $
                   dbfind("bad<4095", $ ; < is really <=
                          dbfind(string("dir=", indir))))
  dbext, entries, "fname, bad, flat_fname, cam_rot, flat_cut", files, badarray, flat_fnames, cam_rots, flat_cuts
  dbclose

  files=strtrim(files)
  nf = N_elements(files)

  ;; Read in an image to set up the bestbias header
  im = ssgread(files[0], hdr)
  sxaddhist, string('(ssg_flatgen.pro) ', systime(/UTC), ' UT'), hdr

  asize=size(im)
  nx=asize[1]
  ny=asize[2]
  total_im = fltarr(nx,ny)
  count_im = fltarr(nx,ny)
  stack_im = fltarr(nf,nx,ny)
  cd,current=cwd
  sxaddpar, hdr, 'PARENTH', getenv("HOST"), ' hostname'
  sxaddpar, hdr, 'PARENTD', cwd, ' current working directory'
  sxaddpar, hdr, 'PARENT', files[0], ' first bias file in directory'

  if keyword_set(TV) then display, total_im, hdr, title = 'Single flat image'
  if keyword_set(showplots) then begin
     window, 6, title='Spectra of rotation corrected and CR rejected flats'
  endif

  err=0
  for i=0,nf-1 do begin
;     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; I want to add in the overclock regions just for the heck of
        ;; it, but it can contaminate the dispersion direction
        ;; spectra, so read in both the data and full image
        full_im = ssgread(files[i], ihdr)
        data_im = ssgread(full_im, ihdr, /data)

        if N_elements(total_im) ne N_elements(full_im) then $
          message, 'ERROR: ' + files[i] + ' and ' + files[0] + ' are not the same size'

        cam_rot = sxpar(ihdr, 'CAM_ROT', count=count)
        if count eq 0 then $
          message, 'ERROR: run ssg_[get & fit]_camrot first'

        ;; Do a preliminary removal of cosmic ray hits and bad pixels

        ;; Generate spectra using median rather than average data,
        ;; since that will throw out cosmic ray hits.  Use the data
        ;; image for the dispersion direction spectrum so that the
        ;; overclock region doesn't skew the median value.
        ssg_spec_extract, data_im, ihdr, spec, junk, med_spec=med_spec
        ssg_spec_extract, full_im, ihdr, junk, xdisp, med_xdisp=med_full_xdisp
        template = template_create(full_im, med_spec, med_full_xdisp)

        ;; Now rotate the template back to the orientation of the
        ;; original images so that we don't smear sharp (bad) features
        ;; in the original images
        template = rot(template, cam_rots[i], cubic=-0.5)

        sigma_im = template_statistic(full_im, template)
        mask_im = mark_bad_pix(sigma_im, cutval=cr_cutval)

        badidx = where(mask_im gt 0, count)

        if count gt 0 then mask_im[badidx] = !values.f_nan
        if keyword_set(TV) then display, full_im+mask_im, ihdr, /reuse
        mask_im = mask_im + 1   ; used multiplicatively below


        ;; Make sure not too much of the array is contaminated 
        if count gt 0.01*N_elements(mask_im) then begin
           badarray[i] = badarray[i] + 4096
           ;; This message is caught by the code above, skipping the
           ;; code below.
           message, 'WARNING: Flat ' + files[i] + ' has more than 1% bright pixels.  Marking it bad in the database'
        endif

        ;; Show spectra if user wants them
        if keyword_set(showplots) then begin
           wset, 6
           ssg_spec_extract, full_im, ihdr, spec, xdisp, /showplots
       endif

        ;; Average: exclude bad pixels from average calculation by
        ;; setting them to 0 before adding AND keeping track of how
        ;; many good pixels were collected at each location
        count_im = count_im + 1
        ;; redefine just in case more code gets added
        badidx = where(finite(mask_im) eq 0, count)
        if count gt 0 then begin 
           full_im[badidx] = 0.
           count_im[badidx] = count_im[badidx] - 1
        endif
        total_im = total_im + full_im
        sxaddhist, string(format='("(ssg_flatgen.pro) added ", a, i6, " bad pixels")', files[i], count), hdr
        message, /INFORMATIONAL, 'added ' + files[i] +  string(count) + ' bad pixels'

        ;;  Array for median
        stack_im[i,*,*] = full_im[*,*]*mask_im
        
     endelse
  endfor
  CATCH, /CANCEL

  if total(count_im) eq 0 then message, 'ERROR: no good flat frames found in ' + indir + ' file ' + outname + ' not written, database ' + dbname + ' not modified.'

  ;; Create flatfield image of total electrons per pixel but taking
  ;; missing pixels into consideration.  
  ;; --> Error image will need some tweaking
  numflats = max(count_im)
  total_im = total_im / count_im * numflats
  sxaddhist, string('(ssg_flatgen.pro) accounted for pixels missing from individual flats'), hdr

  ;; Create median flatfield image
  med_im=fltarr(nx,ny)
  for i=0,nx-1 do begin
     for j=0,ny-1 do begin
        med_im[i,j] = median(stack_im[*,i,j])
     endfor
  endfor
  med_im = med_im* numflats

  ;; Create a template with which to compare the median and average
  ;; images in order to get a stdev
  data_im = ssgread(med_im, ihdr, /data)
  ssg_spec_extract, data_im, ihdr, spec, junk, med_spec=med_spec
  ssg_spec_extract, med_im, ihdr, junk, xdisp, med_xdisp=med_full_xdisp
  template = template_create(med_im, med_spec, med_full_xdisp)
  flat_camrot = mean(cam_rots,/NAN)
  sxaddpar, hdr, 'CAM_ROT', flat_camrot, ' Average flatfield camera rotation'
  template = rot(template, flat_camrot, cubic=-0.5)
  stdev = stddev(total_im-template, /NAN)

  ;; Replace any remaining suspect pixels in the average image with
  ;; median values
  badidx = where(abs(med_im - total_im) gt stdev, count)
  if count gt 0 then begin
     total_im[badidx] = med_im[badidx]
     message, /INFORMATIONAL, 'cleaned up ' + string(count) + ' pixels using median of all bias images'
     sxaddhist, string('(ssg_flatgen.pro) cleaned up ', count, ' pixels'), hdr
  endif

  badidx = where(finite(total_im) eq 0, count)
  if count gt 0 then message, 'WARNING: ' + string(count) + ' bad pixels found in ' + outname + ' Maybe you ought to be using the TRIMSEC.', /CONTINUE


  ;; Handle case where we have a sky flat
  skyflat = readfits(skyflat_name, shdr, silent=silent)
  if N_elements(size(skyflat, /DIMENSIONS)) eq 2 then begin
     skyflat_cut = sxpar(shdr, 'SKYFLATC', COUNT=count)
     if count ne 0 then begin
        message, /CONTINUE, 'Found skyflat ' + skyflat_name + '.  Removing artifacts of flat lamp alignment'
        ;; By construction, there should be no spectral features in
        ;; the skyflat
        good_idx = where(skyflat gt skyflat_cut)
        lamp_and_disp = total_im
        lamp_and_disp[*] = !values.f_nan
        lamp = lamp_and_disp

        ssg_spec_extract, total_im, hdr, spec
        spec_im = template_create(total_im, spec)
        ;; Try to get rid of any spectral features induced by the dust
        ;; on the field lens of the spectrograph
        spec_im=smooth(spec_im, ny/20., /edge_truncate)
        flat_im = total_im/spec_im
        lamp[good_idx] = flat_im[good_idx]/skyflat[good_idx]
        total_im = total_im/lamp

        sxaddhist, string('(ssg_flatgen.pro) Removed flatlamp artifacts with SKYFLAT'), hdr
        sxaddpar, hdr, 'SKYFLAT', skyflat_name, ' skyflat file'
     endif ;; Real skyflat
  endif

;   ;; Normalize
;   good_idx = where(total_im gt flat_cut, count)
;   last_num = 0
;   while last_num ne N_elements(good_idx) do begin
;      if count eq 0 then message, 'ERROR: flat_cut value of ' + string(flat_cut) + ' resulted in no good pixels.  Choose something between 0 and 1'
;      last_num = N_elements(good_idx)
;      total_im = total_im/mean(total_im[good_idx], /NAN)
;      good_idx = where(total_im gt flat_cut, count)
;   endwhile
;   sxaddhist, string('(ssg_flatgen.pro) Normalized image to area inside FLAT_CUT'), hdr

  sxaddpar, hdr, 'FLAT_CUT', flat_cut, ' cut for normalization'

  bad_idx = where(total_im le flat_cut, count)
  if count gt 0 then total_im[bad_idx] = !values.f_nan

  window,6, title=string('Spectra of ', outname)
  ssg_spec_extract, total_im, hdr, spec, xdisp, /showplots
  display, total_im, hdr, title = 'Best flat image'


  ;; Write out the bestflat image
  writefits, outname, total_im, hdr
  message, /INFORMATIONAL, 'Wrote file ' + outname

  ;; Update the bad column in the database, if necessary
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  temp=where(badarray gt 0, count)
  if count gt 0 then begin
     dbupdate, entries, 'bad', badarray
     message, /INFORMATIONAL, 'Recorded bad flat image(s) in ' + dbname
  endif

  entries = dbfind("typecode=[2,5]", dbfind(string("dir=", indir)))
  nf = N_elements(entries)
  flat_fnames = strarr(nf)
  flat_fnames[*] = outname
  dbupdate, entries, 'flat_fname', flat_fnames
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated flat_fnames in ' + dbname
  ;; For convenience 
  cd, indir
  message, /INFORMATIONAL, 'Directory is set to ' + indir


end


