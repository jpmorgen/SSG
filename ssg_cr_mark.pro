;+
; $Id: ssg_cr_mark.pro,v 1.1 2003/06/11 18:12:17 jpmorgen Exp jpmorgen $

; ssg_cr_mark marks cosmic rays in an SSG image and writes a derotated
; image with NANs where the cosmic rays were.  Works best for images
; that do not have spatially varying Doppler structure in the lines
; (e.g. Jupiter might not work well).

;-

pro ssg_cr_mark, indir, flat_cut=flat_cut, tv=tv, showplots=showplots, cr_rate=cr_rate, pix_per_cr=pix_per_cr, cutval=cutval

;  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0

  if NOT keyword_set(cr_rate) then cr_rate = 3E-6 ; per sec per pixel
  if NOT keyword_set(pix_per_cr) then pix_per_cr = 4 ; pixels marked bad per CR hit

  dbclose ;; Just in case
  dbname = 'ssg_reduce'

  dbopen, dbname, 0
  entries = dbfind("typecode=5", $
                   dbfind("bad<2047", $ ; < is really <=
                          dbfind(string("dir=", indir))), count=count)
  if count eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: no object spectra recorded on ' + indir + '.  Returning without doing anything'
     dbclose
     return
  endif


  dbext, entries, "fname, typecode, cr_cut, ncr, nbad", files, typecodes, cr_cuts, num_crs, num_bads
  dbclose
  
  files=strtrim(files)
  nf = N_elements(files)
; DEBUGGING
 cr_cuts=fltarr(nf)


  if keyword_set(showplots) then window,7

  err = 0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; Eventually I'd like to process everything in place, but
        ;; since replacing cosmic rays is time consuming, I have
        ;; done it this way.
        fname = files[i]
        if typecodes[i] eq 5 then begin
           temp=strmid(files[i], 0, strpos(files[i], 'r.fits'))
           fname=temp+'clean.fits'
        endif
        im = ssgread(files[i], hdr, eim, ehdr)
        asize=size(im) & nx=asize[1] & ny=asize[2]
        temp = sxpar(hdr, 'CR_CUT', count=count)
        if count ne 0 then message, 'ERROR: cosmic rays have already been marked.  Start over with ssg_raw_cp'

        cam_rot = sxpar(hdr, 'CAM_ROT', count=count)
        if count eq 0 then message, 'ERROR: file '+ files[i] + ' does not have a CAM_ROT keyword.  You should run ssg_[get&fit]_camrot first'
        temp = sxpar(hdr, 'DISPERS0', count=count)
        if count eq 0 then message, 'ERROR: file '+ files[i] + ' does not have DISPERS* keywords.  You should run ssg_[get&fit]_dispers first'

        ;; Don't mess up header with throw-away operations
        thdr = hdr
        ;; Get initial number of bad pixels
        ibad_idx = where(finite(im+ssg_edge_mask(im, thdr)) eq 0, ibad_pix)

        if keyword_set(TV) then begin
           display, im, hdr, /reuse
           wait, TV/10.
        endif

        sxaddhist, string('(ssg_cr_mark.pro) ', systime(/UTC), ' UT'), hdr

        ;; It is handy but not essential to use the flatfield to cut
        ;; away unneeded edge pixels to increase our signal to noise
        ;; Code shared with ssg_get_slicer
        sli_cent = strtrim(sxpar(hdr,'SLI_CENT',COUNT=count))
        if count eq 0 then begin
           message, 'WARNING: SLI_CENT keyword missing.  Using center of image.  Try running ssg_[get & fit]_sliloc for better results.  Using the center of the image for now', /CONTINUE
           sli_cent = ny/2.
        endif

        ;; Now spot cosmic ray hits.  This is tough, since things like
        ;; comp lines look like cosmic ray hits.  The idea is to vary
        ;; the threshold in sigma space for which a pixel is called a
        ;; cosmic ray hit until you get a reasonable number of hits
        ;; per second
        cr_cuts[i] = 0
        if keyword_set(cutval) then $
          cr_cuts[i] = cutval - 1 $
        else $
          cutval = !values.f_nan

        repeat begin ;; For cut value
           cr_cuts[i] = cr_cuts[i] + 1
           if typecodes[i] eq 2 then cr_cuts[i] = cr_cuts[i] + 3
           tbad_pix = 0         ; total bad pixels
           iter_im = im
           repeat begin ; Iterate to avoid contamination 
              old_tbad_pix = tbad_pix
              if keyword_set (showplots) then $
                wset,7
              ssg_spec_extract, iter_im+ssg_edge_mask(im, thdr), hdr, $
                                spec, junk, med_spec=med_spec, $
                                /AVERAGE, showplots=showplots
              ssg_spec_extract, iter_im, hdr, junk, xdisp, med_xdisp=med_xdisp, /AVERAGE, showplots=showplots
              ;; Create a template, from the median spectra
              template = template_create(iter_im, med_spec, med_xdisp)
              ;; Now munge and rotate that template
              template = ssg_slicer(template, hdr, /DISTORT)
              
              template = ssg_camrot(template, cam_rot, nx/2., sli_cent)

              ;; and compare to the ORIGINAL image
              iter_im=im
              ;; Revisit this
              sigma = template_statistic(iter_im, template);;, /POISSON)
              bad_im = mark_bad_pix(sigma, cutval=cr_cuts[i])
              bad_idx = where(bad_im gt 0, tbad_pix)
              ;; I don't think I really need to worry about previously
              ;; bad pixels, because of the way template statistic,
              ;; etc. work.
              bad_pix = tbad_pix ;- ibad_pix
              print, 'cut value = ', cr_cuts[i], '  num bad_pix = ', bad_pix
              print, 'bad rate per CCD per second', bad_pix/sxpar(hdr, 'DARKTIME')
              if tbad_pix gt 0 then iter_im[bad_idx] = !values.f_nan
              ;;if keyword_set(TV) then display, iter_im, hdr, /reuse
           endrep until tbad_pix ge old_tbad_pix
           num_bads[i] = tbad_pix
           num_crs[i] = tbad_pix/pix_per_cr
        endrep until sxpar(hdr, 'DARKTIME') eq 0 or $
          num_crs[i]/N_elements(im)/sxpar(hdr, 'DARKTIME') lt cr_rate or $
          cr_cuts[i] eq cutval

        ;; Hopefully we have iterated enough to get a good balance of
        ;; cosmic ray hits and not too many good pixels marked as bad
        im = iter_im

;        sxaddhist, '(ssg_cr_mark.pro) found a CUTVAL in sigma space that resulted in ', hdr
;        sxaddhist, '(ssg_cr_mark.pro) a cosmic ray hit rate than CR_RATE', hdr
        
        sxaddpar, hdr, 'CR_CUT', cr_cuts[i], 'in sigma space, minimum cosmic ray hit/bad pixel value'
        sxaddpar, hdr, 'NUMBAD', num_bads[i], 'total number of pixels marked as bad'
;        ;; Strong cosmic ray hits seem to take out about 4 pixels total
;        sxaddpar, hdr, 'NUMCR', pix_per_cr, 'assumed number of bad pixels per cosmic ray hits'

        if keyword_set(TV) then begin
           display, im, hdr, /reuse
           wait, TV/10.
        endif

        if keyword_set(showplots) then begin
           wset,7
           ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /TOTAL, /showplots

        endif
        
        message, /INFORMATIONAL, 'Writing ' + fname
        
        ssgwrite, fname, im, hdr, eim, ehdr

     endelse


  endfor

  CATCH, /CANCEL

  ;; Now write the information to the database
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'cr_cut, ncr, nbad', cr_cuts, num_crs, num_bads

  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated cosmic ray rejection stuff in ' + dbname

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end


