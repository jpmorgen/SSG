;+
; $Id: ssg_flatgen.pro,v 1.4 2002/11/12 21:20:50 jpmorgen Exp $

; ssg_flatgen Generate a bestflat frame from all the flat images in a
; given directory.  Final image is the total number of electrons
; collected in the flatfield process with a little fudging for cosmic
; ray hits.  Overclock region is included so that bias subtraction can
; be checked.

;-
pro ssg_flatgen, indir, outname, showplots=showplots, TV=tv, cr_cutval=cr_cutval, flat_cut=flat_cut, skyflat=skyflat_name, skycut=skycut
;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(outname) then outname = 'bestflat.fits'
  if NOT keyword_set(cr_cutval) then cr_cutval = 5
  ;; For normalization
  if NOT keyword_set(flat_cut) then flat_cut = 0.75
  if NOT keyword_set(sky_cut) then sky_cut = 0.5


  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  lamp_entries = dbfind("typecode=3", $
                        dbfind("bad<4095", $ ; < is really <=
                               dbfind(string("dir=", indir))))
  sky_entries = dbfind("typecode=4", $
                       dbfind("bad<4095", $ ; < is really <=
                              dbfind(string("dir=", indir))), count=num_skyflats)

  if num_skyflats eq 0 then $
    if NOT keyword_set(skyflat_name) then $
    message, 'WARNING: no sky flats found and skyflat keyword not specified.  ' + outname + ' will have some cross-dispersion contamination from the unavoidable flatlamp bore-siting problem'

  ;; Use the same code to build the lamp and sky flats
  repeat begin
     type = 'lamp'
     if num_skyflats gt 0 then type = 'sky'
     entries = lamp_entries
     if num_skyflats gt 0 then entries = sky_entries
     dbext, entries, "fname, bad, flat_fname, cam_rot, flat_cut", files, badarray, flat_fnames, cam_rots, flat_cuts
     files=strtrim(files)
     nf = N_elements(files)
     ;; Read in an image to set up the header
     im = ssgread(files[0], hdr)
     sxaddhist, string('(ssg_flatgen.pro) ', systime(/UTC), ' UT'), hdr
     asize=size(im)
     nx=asize[1]
     ny=asize[2]
     total_im = fltarr(nx,ny)
     count_im = fltarr(nx,ny)
     edge_mask = fltarr(nx,ny)
     stack_im = fltarr(nf,nx,ny)
     cd,current=cwd
     sxaddpar, hdr, 'PARENTH', getenv("HOST"), ' hostname'
     sxaddpar, hdr, 'PARENTD', cwd, ' current working directory'
     sxaddpar, hdr, 'PARENT', files[0], ' first bias file in directory'
     flat_camrot = mean(cam_rots,/NAN)
     sxaddpar, hdr, 'CAM_ROT', flat_camrot, ' Average flatfield camera rotation'
     
     if keyword_set(TV) then display, total_im, hdr, title = 'Single ' + type + ' flat image'
     if keyword_set(showplots) then begin
        window, 7, title='Spectra of rotation corrected and CR rejected ' + type + ' flats'
     endif

     err=0
     for i=0,nf-1 do begin
;     CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           im = ssgread(files[i], ihdr)
           if N_elements(total_im) ne N_elements(im) then $
             message, 'ERROR: ' + files[i] + ' and ' + files[0] + ' are not the same size'
           cam_rot = sxpar(ihdr, 'CAM_ROT', count=count)
           if count eq 0 then $
             message, 'ERROR: run ssg_[get & fit]_camrot first'

           cut = flat_cut
           if num_skyflats gt 0 then cut = sky_cut
           bad_idx = where(im lt cut * im, count)
           if count gt 0 then edge_mask[bad_idx] = !values.f_nan

           ;; Do a preliminary removal of cosmic ray hits and bad
           ;; pixels.  pass ssg_spec_extract ihdr so it does
           ;; de-rotation appropriate for this specific image
           ssg_spec_extract, im + edge_mask, ihdr, spec, xdisp, $
                             med_spec=med_spec, med_xdisp=med_xdisp, $
                             /TOTAL
           template = template_create(im, med_spec, med_xdisp)

           ;; Now rotate the template back to the orientation of the
           ;; original images so that we don't smear sharp (bad) features
           ;; in the original images
           template = rot(template, cam_rots[i], cubic=-0.5)

           sigma_im = template_statistic(im, template)
           ;; mark_bad_pix does not return NAN, but rather the number
           ;; of times a pixel is found to be bad as a function of
           ;; size scale
           cr_mask = mark_bad_pix(sigma_im, cutval=cr_cutval)
           cd_idx = where(cr_mask gt 0, cr_pix)
           if cr_pix gt 0 then cr_mask[cd_idx] = !values.f_nan

           ;; Skyflats: create a template with which to divide out any
           ;; spectral features
           if num_skyflats gt 0 then begin
              ssg_spec_extract, im, ihdr, spec, /TOTAL
              template = template_create(im, spec)
              im = im/template
           endif

           if keyword_set(TV) then display, im+cr_mask, ihdr, /reuse
           cr_mask = cr_mask + 1 ; used multiplicatively for stack_im

           ;; Show spectra if user wants them
           if keyword_set(showplots) then begin
              wset, 7
              ssg_spec_extract, im+edge_mask+cr_mask, ihdr, /showplots, /TOTAL
           endif

           ;; Make sure not too much of the array is contaminated 
           if cr_pix gt 0.01*N_elements(im) then begin
              badarray[i] = badarray[i] + 4096
              message, 'WARNING: Flat ' + files[i] + ' has more than 1% bright pixels.  Marking it bad in the database'
           endif

           ;; OK, it is safe to add this image to the total, keeping
           ;; track of pixels lost to cosmic ray hits
           count_im = count_im + 1
           if cr_pix gt 0 then begin 
              im[cd_idx] = 0.
              count_im[cd_idx] = count_im[cd_idx] - 1
           endif
           total_im = total_im + im
           sxaddhist, string(format='("(ssg_flatgen.pro) added ", a, i6, " bad pixels")', files[i], cr_pix), hdr
           message, /INFORMATIONAL, 'added ' + files[i] +  string(cr_pix) + ' bad pixels'

           ;; Make an array for the median calculation
           stack_im[i,*,*] = im[*,*] * cr_mask
           
        endelse
     endfor
     CATCH, /CANCEL

     ;; Polish up the total image
     if total(count_im) eq 0 then begin
        message, 'WARNING: no good ' + type + ' flat frames found in ' + indir
     endif else begin ;; We have something to process
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
        med_im = med_im * numflats

        ;; Create a template from the median image with which to
        ;; compare the median and average images in order to get a
        ;; stdev.
        ssg_spec_extract, med_im, hdr, spec, xdisp, $
                          med_spec=med_spec, med_xdisp=med_xdisp, $
                          /TOTAL
        template = template_create(med_im, med_spec, med_xdisp)
        template = rot(template, flat_camrot, cubic=-0.5)
        stdev = stddev(total_im-template, /NAN)

        ;; Replace any remaining suspect pixels in the average image with
        ;; median values
        badidx = where(abs(med_im - total_im) gt stdev, count)
        if count gt 0 then begin
           total_im[badidx] = med_im[badidx]
           message, /INFORMATIONAL, 'cleaned up ' + string(count) + ' pixels using median of all flat images'
           sxaddhist, string('(ssg_flatgen.pro) cleaned up ', count, ' pixels'), hdr
        endif

        badidx = where(finite(total_im) eq 0, count)
        if count gt 0 then message, 'WARNING: ' + string(count) + ' bad pixels found in ' + write_name + ' Maybe you ought to be using the TRIMSEC.', /CONTINUE

        if num_skyflats gt 0 then begin ;; sky flat
           if NOT keyword_set(skyflat_name) then skyflat_name = 'skyflat.fits'
           write_name = skyflat_name
           ;; The sky flat is supposed to record the true
           ;; cross-disperion response of the slicer + the lamp flat
           ;; the true dispersion flat.  So, use the median
           ;; cross-dispersion spectrum to create a template that
           ;; becomes our sky flat
           ssg_spec_extract, total_im, hdr, med_xdisp=med_xdisp, /TOTAL
           total_im = template_create(total_im, med_xdisp)
           total_im = rot(total_im, flat_camrot, cubic=-0.5)
           sxaddhist, string('(ssg_flatgen.pro) Created slicer response from median cross-disp spectrum'), hdr
           sxaddpar, hdr, 'SKY_CUT', sky_cut, 'cut for normalization'

           num_skyflats = 0
        endif $ ;; sky flat
        else begin ;; lamp flat
           write_name = outname
           ;; Use the sky flat to improve the lamp flat
           skyflat = readfits(skyflat_name, shdr, silent=silent)
           if N_elements(size(skyflat, /DIMENSIONS)) eq 2 then begin
              message, /CONTINUE, 'Found skyflat ' + skyflat_name + '.  Removing artifacts of flat lamp alignment'
              sky_cut = sxpar(shdr, 'SKY_CUT', COUNT=count)
              if count eq 0 then begin
                 message, /CONTINUE, 'WARNING: sky flat file ' + skyflat_name + ' did not have a SKY_CUT parameter.  Was it created properly?  Using FLAT_CUT for normalization instead'
                 sky_cut = flat_cut
              endif
              ;; By construction, there should be no spectral features in
              ;; the skyflat.  We want to use it to correct for any
              ;; subtle shape variation in the cross-dispersion
              ;; direction induced by the unavoidable (without lots of
              ;; construction work) bore-siting problem with the flat lamp
              skyflat = normalize(skyflat, sky_cut)
              bad_idx = where(skyflat le sky_cut)
              ;; Smooth out the 2D structure in the lamp flat, which
              ;; is probably dust on the field lens and/or
              ;; non-uniformities in the CCD.  Soomthing scale is
              ;; approximately 1/2 a slicer width
              lamp_im = smooth(total_im / skyflat, ny/20, /edge_truncate)
              ;; This still has the white light response in it
              ssg_spec_extract, lamp_im, hdr, $
                                lamp_spec, lamp_xdisp, /TOTAL
              ;; Make a lamp_im that is just cross-dispersion
              lamp_im = normalize(template_create(total_im, lamp_xdisp))
              lamp_im = rot(lamp_im, flat_camrot, cubic=-0.5)
              good_idx = where(skyflat gt sky_cut)
              total_im[good_idx] = total_im[good_idx] / lamp_im[good_idx]

              sxaddhist, string('(ssg_flatgen.pro) Removed flatlamp artifacts with SKYFLAT'), hdr
              sxaddpar, hdr, 'SKY_FLAT', skyflat_name, ' skyflat file'
           endif ;; found skyflat
           sxaddpar, hdr, 'FLAT_CUT', flat_cut, 'cut for normalization'
           num_skyflats = -1
        endelse ;; lamp flat

        window,6, title=string('Spectra of ', write_name)
        ssg_spec_extract, total_im, hdr, /showplots, /TOTAL
        display, total_im, hdr, title = write_name

        ;; Write file
        writefits, write_name, total_im, hdr
        message, /INFORMATIONAL, 'Wrote file ' + write_name

        ;; Update the bad column in the database, if necessary
        oldpriv=!priv
        !priv = 2
        dbopen, dbname, 1
        temp=where(badarray gt 0, count)
        if count gt 0 then begin
           dbupdate, entries, 'bad', badarray
           message, /INFORMATIONAL, 'Recorded bad flat image(s) in ' + dbname
        endif

     endelse ;; We have something to process
  endrep until num_skyflats eq -1

  ;; For convenience 
  cd, indir
  message, /INFORMATIONAL, 'Directory is set to ' + indir


end


