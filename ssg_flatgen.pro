;+
; $Id: ssg_flatgen.pro,v 1.7 2003/03/10 18:31:01 jpmorgen Exp $

; ssg_flatgen Generate a bestflat frame from all the flat images in a
; given directory.  Final image is the total number of electrons
; collected in the flatfield process with a little fudging for cosmic
; ray hits.  Overclock region is included so that bias subtraction can
; be checked.

;-
pro ssg_flatgen, indir, showplots=showplots, TV=tv, cr_cutval=cr_cutval, flat_cut=flat_cut, skyflat_name=skyflat_name, skycut=skycut
;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(cr_cutval) then cr_cutval = 5
  ;; For normalization
  if NOT keyword_set(flat_cut) then flat_cut = 0.1
  if NOT keyword_set(sky_cut) then sky_cut = flat_cut
  if NOT keyword_set(sli_cut) then sli_cut = flat_cut

  solid=0
  dotted=1
  dashed=2
  dash_dot=3
  dash_3dot = 4
  long_dash=5

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
;  ;; DEBUGGING
;  sky_entries = -1
;  num_skyflats = 0

  all_entries = lamp_entries
  if num_skyflats gt 0 then begin
     all_entries = [lamp_entries, sky_entries]     
  endif else begin
     if NOT keyword_set(skyflat_name) then begin
        skyflat_name = 'NONE'
        message, 'WARNING: no sky flats found and SKYFLAT keyword not specified.  Lamp flats will have some cross-dispersion contamination from the unavoidable flatlamp bore-siting problem', /CONTINUE
     endif
  endelse
  num_slicer_flats = N_elements(all_entries)
  num_dust_flats = num_slicer_flats

  ;; Use the same code to build the flatfield images, since so much
  ;; code is common.  We already have the slicer center and canmera
  ;; rotation at this point, but we have not removed cosmic rays.

  ;; Sequence.  First we want to read in the images without moving or
  ;; rotating them to be able to get rid of cosmic-ray hits.  As we
  ;; progress with better and better flats, we might want to divide
  ;; those out first to get even better cosmic-ray rejection.  For
  ;; each group of flatfields, sky and lamp, we want to make separate
  ;; slicer, dust, and light-source flats.  The slicer flat is formed
  ;; by dividing out the spectral shape and doing a median
  ;; cross-dispersion extraction.  The resulting spectrum is turned
  ;; into a template which is shifted to the average flatfield
  ;; position and rotation and stacked.  This is then the sky_ or
  ;; lamp_slicer.flat.  The next time around the repeat loop, this is
  ;; properly oriented and divided into each image before cosmic rays
  ;; are removed.  Spectral shape is again divided out and the result
  ;; is stacked _without_ translation or rotation to form the sky_ and
  ;; lamp_dust.flat.  These flats should be similar + might eventually
  ;; be added together.  At the very least, the user will have to
  ;; specify which _dust.flat to use for the final bestflat.  The third
  ;; time through the repeat loop, the dust is divided, out, the
  ;; reoriented light-source_slicer shape is divided, but the spectral
  ;; shape is not.  This only makes sense in the case of the lamp
  ;; flat, but might as well do it for both for the heck of it.  These
  ;; are stacked onto the average center and rotation orientation to
  ;; form lamp_source and sky_source flats.  These only should vary in
  ;; the dispersion direction (after average flat rotation is taken
  ;; out).

  ;; The flatfielding process then has three steps.  First dividing
  ;; out the best dust.flat without any translation or rotation.  Next
  ;; a translated and rotated sky_slicer.flat is divided, if
  ;; available, otherwise the lamp_slicer.flat is used.  Finally, the
  ;; lamp_source.flat is divided.

  repeat begin
     ;; Lamp or sky loop
     type = 'lamp'
     entries = lamp_entries
     if num_skyflats gt 0 then begin
        type = 'sky'
        entries = sky_entries
     endif

     subtype = 'slicer'
     repeat begin
        ;; slicer, dust, source loop
        write_name = string(type+'_'+subtype+'_flat.fits')
        ;; In case of previous error
        dbclose
        dbopen, dbname, 0
        dbext, entries, 'fname, bad, m_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, m_cam_rot, cam_rot', $
         files, badarray, m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, m_cam_rots, cam_rots

        files=strtrim(files)
        nf = N_elements(files)

        ;; Read in an image to set up the header
        im = ssgread(files[0], hdr, eim, ehdr)
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
        sxaddpar, hdr, 'RAWFILE', 'NONE', ' Product of reduced files in PARENTD'
        sxaddpar, hdr, 'SSGFILE', write_name, ' Name of this file'
        flat_sli_cen = mean(sli_cents,/NAN)
        sxaddpar, hdr, 'SLI_CENT', flat_sli_cen, ' Average ' + type + ' flat slicer center'
        flat_camrot = mean(cam_rots,/NAN)
        sxaddpar, hdr, 'CAM_ROT', flat_camrot, ' Average  ' + type + ' flat camera rotation'
        sxaddhist, string('(ssg_flatgen.pro) Modified SLI_CENT, CAM_ROT, and PARENT*'), hdr
        sxaddpar, hdr, 'FLATTYPE', type, 'Flatfield source (sky or lamp)'
        sxaddpar, hdr, 'FSUBTYPE', subtype, 'Flafield subtype (slicer, dust, source)'
        
        if keyword_set(TV) then display, total_im, hdr, title = 'Single ' + type + ' flat image'
        if keyword_set(showplots) then begin
           window, 7, title='TOTAL spectra of rotation corrected and CR rejected ' + type + ' flats'
        endif

        err=0
        for i=0,nf-1 do begin
;           CATCH, err
           message, 'Processing ' + type + ' ' + subtype + ' flat ' + files[i], /CONTINUE
           if err ne 0 then begin
              message, /NONAME, !error_state.msg, /CONTINUE
              message, 'skipping ' + files[i], /CONTINUE
              dbclose
           endif else begin
              im = ssgread(files[i], ihdr, eim, iehdr)
              if N_elements(total_im) ne N_elements(im) then $
                message, 'ERROR: ' + files[i] + ' and ' + files[0] + ' are not the same size'
              cam_rot = sxpar(ihdr, 'CAM_ROT', count=count)
              if count eq 0 then $
                message, 'ERROR: run ssg_[get & fit]_camrot first'

              
              ;; Do a prelimiary cut to find the edges + keep track of
              ;; these in a separate array, edge_mask.  Also put any
              ;; bad columns, etc. into edge_mask
              cut = flat_cut
              if num_skyflats gt 0 then cut = sky_cut
              edge_idx = where(normalize(im, cut) lt cut or $
                               finite(im) eq 0, count, complement=middle_idx)
              if count gt 0 then edge_mask[edge_idx] = !values.f_nan

              ;; COSMIC RAY REMOVAL.  Make a flat image, using any
              ;; past products to get better contrast on the cosmic
              ;; rays
              fim = im
              if subtype eq 'dust' then begin
                 ;; Slicer flat needs to be aligned to each image.
                 saim = ssg_flat_align(im, ihdr, slicer_flat, hdr)
                 fim[middle_idx] = im[middle_idx] / saim[middle_idx]
              endif
              if subtype eq 'source' then begin
                 ;; Dust flat should not be re-aligned
                 fim[middle_idx] = im[middle_idx] / dust_flat[middle_idx]
                 saim = ssg_flat_align(im, ihdr, slicer_flat, hdr)
                 fim[middle_idx] = fim[middle_idx] / saim[middle_idx]
              endif

              ;; Display raw image, pause and display flattened image
              ;; which we will use to spot cosmic rays
              if keyword_set(TV) then begin
                 display, im, ihdr, /reuse
                 wait, 0.5
                 display, fim, ihdr, /reuse
                 wait, 0.5
              endif

              ;; Display histogram of Poisson statistics.  Do this
              ;; before removal of cosmic rays so we get some high
              ;; count rates too.
;              sqim = sqrt(im+edge_mask)
;              window,5
;              plot, histogram(sqim, MIN=0, MAX=100, NBINS=100, /NAN)
;              oplot, histogram(im+edge_mask, MIN=0, MAX=10000, NBINS=100, /NAN), linestyle=dotted
;
;              
              ;; Now make a full 2D template against which cosmic rays
              ;; should stand out well.  Pass ssg_spec_extract ihdr so
              ;; it does extraction de-rotation appropriate for this
              ;; specific image.  Use the edge_mask to get the best
              ;; dispersion direction, but use the full image to get
              ;; the cross-dispersion right
              ssg_spec_extract, fim + edge_mask, ihdr, fspec, $
                                med_spec=fmed_spec, $
                                /AVERAGE
              ssg_spec_extract, fim, ihdr, junk, fxdisp, $
                                med_xdisp=fmed_xdisp, $
                                /AVERAGE
              template = template_create(fim, fmed_spec, fmed_xdisp)
; This didn't help POSSION problem
;           template = template_create(im, spec, xdisp)

              ;; Rotate the template to overlap the original image
              ;; rather than the other way around, so that we don't
              ;; smear sharp features in the original images
              template = ssg_camrot(template, cam_rots[i], nx/2., sli_cents[i])

; This made two peaks at around 120 sigma.  I think this is trying to
; tell me something about Poisson statistics, but I am just being dense!
;           ;; Make sure template is really close to image in
;           ;; normalization
;           template = template*mean(im,/NAN)/mean(template,/NAN)

              ;; --> experiment with this again.

              sigma_im = template_statistic(fim, template);;, /POISSON)

              ;; Need to get rid of NANs for mark_bad_pix.  Put them
              ;; back in below
              bad_idx = where(finite(sigma_im) eq 0, snan_count)
              if snan_count gt 0 then sigma_im[bad_idx] = 0

              ;; mark_bad_pix does not return NAN, but rather the number
              ;; of times a pixel is found to be bad as a function of
              ;; size scale
              cr_mask = mark_bad_pix(abs(sigma_im), cutval=cr_cutval)
              cr_idx = where(cr_mask gt 0, cr_count)
              if cr_count gt 0 then cr_mask[cr_idx] = !values.f_nan
;              ;; Put back in pixels that were marked as NAN above
;              if snan_count gt 0 then cr_mask[bad_idx] = !values.f_nan

              ;; Now get a better edge mask.  I could iterate, but
              ;; this should be more than sufficient, since cosmic
              ;; rays were unlikely to significantly effect the
              ;; normalization in the first edge mask, which helped
              ;; mark cosmic rays....
              edge_mask[*] = 0.
              bad_idx = where(normalize(im, cut) lt cut or $
                              finite(im) eq 0, edge_count)
              if edge_count gt 0 then edge_mask[bad_idx] = !values.f_nan

              ;; SLICER FLAT.  The slicer flat is basically a 2D
              ;; version of the median cross-dispersion spectrum.
              ;; Lets flatten the image first in the spectral
              ;; direction, though, before we take a median.
              if subtype eq 'slicer' then begin
                 ;; By using edge_mask here, we get a better average spectrum
                 ssg_spec_extract, im+cr_mask+edge_mask, ihdr, spec, /AVERAGE
                 template = template_create(im, normalize(spec))
                 im = im/template
                 ;; But lets leave the edges in for cross-dispersion
                 ;; direction.  
                 ssg_spec_extract, im+cr_mask, ihdr, med_xdisp=med_xdisp, $
                                   /AVERAGE
                 im = template_create(im, med_xdisp)
                 ;; No additional marking of cosmic rays is needed
                 ;; since we are forming the image from a template
                 ;; Align the image with the average slicer center and
                 ;; camera rotation.
;                 im = ssg_flat_align(im, hdr, im, ihdr)
                 im = ssg_camrot(im, flat_camrot, nx/2., flat_sli_cen)
              endif ;; slicer flat

              ;; DUST FLAT.  We have already created a
              ;; slicer-flattened image, fim.  It has the spectral
              ;; shape in it, which we should divide out.  Divide out
              ;; a smoothed version of the spectral shape so dust
              ;; features don't get divided.  Smooth over the size
              ;; scale of approximately 1/2 a slicer width
              if subtype eq 'dust' then begin
                 im = fim
                 im = im + edge_mask + cr_mask
                 ssg_spec_extract, im, ihdr, spec, /AVERAGE
                 spec = smooth(spec, ny/20, /NAN)
                 template = template_create(im, normalize(spec))
                 im = im/template
              endif

              ;; SOURCE FLAT.  We have already created a slicer- and
              ;; dust-flattened image, fim.  Mark bad pixels and then
              ;; translate and rotate onto the average flat
              ;; position/rotation 
              if subtype eq 'source' then begin
                 im = fim
                 im = im + edge_mask + cr_mask
                 im = ssg_flat_align(im, hdr, im, ihdr)
              endif

              if keyword_set(TV) then display, im, ihdr, /reuse

              ;; Show spectra if user wants them
              if keyword_set(showplots) then begin
                 wset, 7
                 ssg_spec_extract, im+edge_mask+cr_mask, ihdr, $
                                   /showplots, /TOTAL
              endif

              ;; Make sure not too much of the array is contaminated 
              if cr_count gt 0.01*N_elements(im) then begin
                 badarray[i] = badarray[i] OR 16384
                 message, 'WARNING: Flat ' + files[i] + ' has more than 1% bright pixels.  Marking it bad in the database'
              endif

              ;; OK, it is safe to add this image to the total, keeping
              ;; track of pixels lost to cosmic ray hits and the edges
              ;; (in the case of dust flats, the edges can move)
              good_idx = where(finite(im) eq 1, count, complement=bad_idx)
              if count eq 0 then message, $
                'ERROR: no good pixels left in ' + files[i]
              count_im[good_idx] = count_im[good_idx] + 1
              total_im[good_idx] = total_im[good_idx] + im[good_idx]

              message, /INFORMATIONAL, 'added ' + type + ' ' + subtype + ' flat ' + files[i]
              print, ' rotated ' + string(flat_camrot - cam_rots[i]) + ' deg, translated ' + string(flat_sli_cen - sli_cents[i]) + ' pixles, ' + string(cr_count+edge_count) + ' bad pixels'

              ;; Make an array for the median calculation
              stack_im[i,*,*] = im[*,*]
              
           endelse  ;; error condition
        endfor ;; Each flatfield file
        CATCH, /CANCEL

        ;; Polish up the total image
        if total(count_im) eq 0 then begin
           message, 'WARNING: no good ' + type + ' ' + subtype + ' flat frames found in ' + indir
        endif else begin ;; We have something to process
           numflats = max(count_im)
           total_im = total_im / count_im * numflats
           sxaddhist, string('(ssg_flatgen.pro) accounted for pixels missing from individual flats'), hdr

           ;; Create median flatfield image.  Mark missing pixels with
           ;; NAN so median ignores them.  Unlike the bias, we can use
           ;; Poisson statistics of the total image to get the error estimate 
           bad_idx = where(stack_im eq 0, count)
           if count gt 0 then $
             stack_im[bad_idx] = !values.f_nan
           med_im=fltarr(nx,ny)
           for i=0,nx-1 do begin
              for j=0,ny-1 do begin
                 med_im[i,j] = median(stack_im[*,i,j])
                 ;;;;;;;eim[i,j] = stddev(stack_im[*,i,j])
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
           ;; Don't slide dust things around
           if subtype ne 'dust' then begin
              template = ssg_camrot(template, flat_camrot, nx/2., flat_sli_cen)
           endif
           stdev = stddev(total_im-template, /NAN)

           ;; Replace any remaining suspect pixels in the average image with
           ;; median values
           badidx = where(abs(med_im - total_im) gt stdev, count)
           if count gt 0 then begin
              total_im[badidx] = med_im[badidx]
              message, /INFORMATIONAL, 'cleaned up ' + string(count) + ' pixels using median of all flat images'
              sxaddhist, string('(ssg_flatgen.pro) cleaned up ', count, ' pixels'), hdr
           endif

;        badidx = where(finite(total_im) eq 0, total_count)
;        if count gt 0 then message, 'WARNING: ' + string(count) + ' bad pixels found in ' + write_name + ' Maybe you ought to be using the TRIMSEC.', /CONTINUE

           ;; SKY FLAT
           if type eq 'sky' then begin
              ;; The sky flat is supposed to record the true
              ;; cross-disperion response of the slicer + the lamp flat
              ;; the true dispersion flat.  So, use the median
              ;; cross-dispersion spectrum to create a template that
              ;; becomes our sky flat
              sxaddpar, hdr, 'SKY_CUT', sky_cut, 'cut for normalization'

              num_skyflats = 0
           endif  ;; sky flat

           ;; LAMP FLAT
           if type eq 'lamp' then begin
;              ;; Use the sky flat to improve the lamp flat
;              skyflat = readfits(skyflat_name, shdr, silent=silent)
;              if N_elements(size(skyflat, /DIMENSIONS)) eq 2 then begin
;                 message, /CONTINUE, 'Found skyflat ' + skyflat_name + '.  Removing artifacts of flat lamp alignment'
;                 sky_cut = sxpar(shdr, 'SKY_CUT', COUNT=count)
;                 if count eq 0 then begin
;                    message, /CONTINUE, 'WARNING: sky flat file ' + skyflat_name + ' did not have a SKY_CUT parameter.  Was it created properly?  Using FLAT_CUT for normalization instead'
;                    sky_cut = flat_cut
;                 endif
;                 ;; By construction, there should be no spectral features in
;                 ;; the skyflat.  We want to use it to correct for any
;                 ;; subtle shape variation in the cross-dispersion
;                 ;; direction induced by the unavoidable (without lots of
;                 ;; construction work) bore-siting problem with the flat lamp
;                 skyflat = normalize(skyflat, sky_cut)
;                 bad_idx = where(skyflat le sky_cut, complement=good_idx)
;                 ;; Try to get at the true lamp shape.  Since we have
;                 ;; totally smoothed the skyflat in the dispersion
;                 ;; direction, any CCD or other 2D cosmetic features,
;                 ;; which I will call 'dust' will remain when we do this
;                 ;; division: 
;                 lamp_and_dust = total_im / skyflat
;                 lamp_and_dust[bad_idx] = !values.f_nan
;                 ;; To get at the lamp shape, we can smooth out the 2D
;                 ;; structure in this image.  Lets use a soomthing scale
;                 ;; that is approximately 1/2 a slicer width
;                 lamp_im = smooth(lamp_and_dust, ny/20, /NAN)
;                 ;; But this still has the white light response in it.
;                 ;; Just like the skyflat, we want a lamp_im that is just
;                 ;; the cross-dispersion variation
;                 ssg_spec_extract, lamp_im, hdr, $
;                                   lamp_spec, lamp_xdisp, /TOTAL
;;--> wrong
;                 lamp_im = normalize(template_create(total_im, lamp_xdisp))
;                 lamp_im = ssg_camrot(total_im, flat_camrot, nx/2.,flat_sli_cen)
;                 good_idx = where(skyflat gt sky_cut)
;                 total_im[good_idx] = total_im[good_idx] / lamp_im[good_idx]
;
;                 sxaddhist, string('(ssg_flatgen.pro) Removed flatlamp artifacts with SKYFLAT'), hdr
;                 sxaddpar, hdr, 'SKY_FLAT', skyflat_name, ' skyflat file'
;              endif ;; found skyflat
              sxaddpar, hdr, 'FLAT_CUT', flat_cut, 'cut for normalization'
              num_skyflats = -1
           endif ;; lamp flat

           window,6, title=string('TOTAL spectra of ', write_name)
           ssg_spec_extract, total_im, hdr, /showplots, /TOTAL
           display, total_im, hdr, title = write_name

           ;; Write file
           ssgwrite, write_name, total_im, hdr, sqrt(total_im), ehdr
           message, /INFORMATIONAL, 'Wrote file ' + write_name

           case subtype of
              'slicer': begin
                 slicer_flat = normalize(total_im, cut)
                 if num_skyflats ne -1 then skyflat_name = write_name
                 subtype = 'dust'
              end
              'dust': begin
                 dust_flat = normalize(total_im, cut)
                 subtype = 'source'
              end
              'source': begin
                 subtype = 'DONE'
              end
           endcase


        endelse ;; We have something to process
     endrep until subtype eq 'DONE'

     ;; Update the bad column in the database, if necessary
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     temp=where(badarray gt 0, count)
     if count gt 0 then begin
        dbupdate, entries, 'bad', badarray
        message, /INFORMATIONAL, 'Recorded bad flat image(s) in ' + dbname
     endif

  endrep until num_skyflats eq -1

  ;; For convenience 
  cd, indir
  message, /INFORMATIONAL, 'Directory is set to ' + indir
  dbclose


end


