;+
; $Id: ssg_get_slicer.pro,v 1.6 2003/06/13 03:52:14 jpmorgen Exp $

; ssg_get_slicer.  derive slicer shape parameters and record them in
; the database

;-

function slicer_compare, in_slicer, image_or_dp, image=im_or_fname, hdr=hdr, slicer_size=slicer_size, blocking=blocking, cr_cutval=cr_cutval, noninteractive=noninteractive

  ;; Get ready to use this function in a variety of contexts
  if n_params() eq 2 then begin
     if N_elements(in_slicer) eq N_elements(image_or_dp) then $
       message, 'ERROR: I think you are asking me to calculate a derivative of the parameters for tnmin.  I don''nt know how to do this.  Make sure you specify /AUTODERIVATIVE with tnmin'
     if keyword_set(im_or_fname) then $
       message, 'WARNING: both the image parameter and keywords are set, defaulting to image keyword (image=...)' $
     else $
       im_or_fname = image_or_dp
  endif

  if N_elements(im_or_fname) eq 0 then $
    message, 'ERROR: no filename or image supplied'
  if size(im_or_fname, /TNAME) eq 'STRING' then $
    im=ssgread(im_or_fname, hdr) $
  else im = im_or_fname
  if N_elements(size(im, /DIMENSIONS)) ne 2 then $
    message, 'ERROR: specify a valid filename or a 2D array to display.'

  ;; If we were called by tnmin, in_slicer is a 1D array.  We might
  ;; need to make it a 2D array so dispersion dependent distortions
  ;; get handled
  slicer = in_slicer
  if N_elements(size(in_slicer, /DIMENSIONS)) ne 2 then begin
     if N_elements(slicer_size) eq 0 then begin
        message, 'WARNING: slicer_size keyword not supplied, assuming correction is a function of cross-dispersion direction only', /CONTINUE
        slicer_size=[N_elements(in_slicer)]
     endif
     slicer=fltarr(slicer_size[0])
     if N_elements(slicer_size gt 1) then begin
        if slicer_size[0] * slicer_size[1] ne N_elements(in_slicer) then $
          message, 'ERROR: input slicer array size is inconsistent with the slicer_size keyword value'
        slicer=fltarr(slicer_size[0], slicer_size[1])
     endif
     slicer[*]=in_slicer[*]
  endif

  ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, slicer=slicer, blocking=blocking, cam_rot=0, /AVERAGE
  ref_im=template_create(im, med_spec, xdisp)
  ref_im = ssg_slicer(ref_im, hdr, slicer=slicer, /DISTORT)

  if NOT keyword_set(noninteractive) then begin
     answer = strupcase(get_kbrd(0))
     if answer eq 'D' then begin
        display, im, /reuse
        display, ref_im, /reuse
     endif
     if answer eq 'S' then message, 'STOPPING FIT'
  endif
  return, total(im*ref_im, /NAN)
end

pro ssg_get_slicer, indir, VERBOSE=verbose, TV=tv, zoom=zoom, slicer=slicer_in, blocking=blocking, flat_cut=flat_cut, cr_cutval=cr_cutval, showplots=showplots, noninteractive=noninteractive, review=review, write=write, winnum=winnum

;  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0
  if NOT keyword_set(cr_cutval) then cr_cutval = 10
  if NOT keyword_set(winnum) then winnum = 6

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  ;; Get all the files in the directory so we can mark camrot as not
  ;; measured on the ones where we can't measure it.
  entries = dbfind(string("dir=", indir))

  dbext, entries, "fname, nday, date, typecode, m_slice, bad", files, ndays, dates, typecodes, slicers, badarray
  files=strtrim(files)
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time


  if NOT keyword_set(review) then begin ; We really want to do all the fitting

     slicers[*] = !values.f_nan
     
     if NOT keyword_set(slicer_in) then slicer_in=0
     if keyword_set(showplots) then begin
        ;;window,2, title='Raw comp spectra'
        window,winnum, title='Cleaned comp spectra'
     endif

     ngood = 0
     err=0

     for i=0,nf-1 do begin
        message, 'Looking at ' + files[i], /CONTINUE
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           if badarray[i] gt 4095 then message, 'Bad = ' + string(badarray[i])
           if typecodes[i] ne 2 then message, 'Can only get a slicer shape from comps at the moment'

           im = ssgread(files[i], hdr, /DATA, /TRIM)
           asize=size(im) & nx=asize[1] & ny=asize[2]
           sli_cent = strtrim(sxpar(hdr,'SLI_CENT',COUNT=count))
           if count eq 0 then begin
              message, 'ERROR: SLI_CENT keyword missing.  Using center of image.  Try running ssg_[get & fit]_sliloc for better results.';  Using the center of the image for now', /CONTINUE
              sli_cent = ny/2.
           endif
           sli_bot = float(sxpar(hdr,'SLI_BOT',COUNT=count))
           if count eq 0 then begin
              message, 'WARNING: SLI_BOT keyword missing.  Try running ssg_[get & fit]_sliloc for better results', /CONTINUE
              sli_bot = 0
           endif
           sli_top = float(sxpar(hdr,'SLI_TOP',COUNT=count))
           if count eq 0 then begin
              message, 'WARNING: SLI_TOP keyword missing.  Try running ssg_[get & fit]_sliloc for better results', /CONTINUE
              sli_top = ny-1
           endif

           if NOT keyword_set(blocking) then $
             blocking = round((sli_top - sli_bot)/20.)

           ;; Remove cosmic ray hits.--try it without
           edge_mask = ssg_edge_mask(im, hdr)
;           ssg_spec_extract, im + edge_mask, hdr, spec, xdisp, $
;                             med_spec=med_spec, med_xdisp=med_xdisp, $
;                             slicer=slicer, /AVERAGE
;           template = template_create(im, med_spec, xdisp)
;           template = ssg_slicer(im, hdr, slicer=slicer, /DISTORT)
;           sigma_im = template_statistic(im, template, /POISSON)
;           mask_im = mark_bad_pix(sigma_im, cutval=cr_cutval)
;           badidx = where(mask_im gt 0, count)
;           if count gt 0 then im[badidx] = !values.f_nan
           ;; Now that we have danced around the NAN problem in
           ;; mark_bad_pix, we can put all the NANs into im
           im = im + edge_mask
           
           ;; Derotate
           cam_rot = strtrim(sxpar(hdr,'CAM_ROT',COUNT=count))
           if count eq 0 then begin
              message, 'WARNING: CAM_ROT keyword missing.  Using 0', /CONTINUE
              cam_rot = 0
           endif
           im = ssg_camrot(im, -cam_rot, nx/2., sli_cent)

           ;; Make a smaller image that includes just good rows.  
           firsty = 0
           repeat begin
              temp = where(finite(im[*,firsty]) eq 1, count)
              if count eq 0 then firsty = firsty + 1
           endrep until count ne 0
           lasty = ny-1
           repeat begin
              temp = where(finite(im[*,lasty]) eq 1, count)
              if count eq 0 then lasty = lasty - 1
           endrep until count ne 0

           im = im[*,firsty:lasty]

           ;; Now adjust the reference point of SLI* to reflect the
           ;; trimmed rows so that ssg_slicer functions properly
           sli_bot = sli_bot - firsty
           if sli_bot lt 0 then sli_bot = 0
           sli_top = sli_top - firsty
           if sli_top gt ny then sli_top = ny
           sli_cent = sli_cent - firsty
           sxaddpar, hdr, 'SLI_BOT', sli_bot
           sxaddpar, hdr, 'SLI_TOP', sli_top
           sxaddpar, hdr, 'SLI_CENT', sli_cent
           asize=size(im) & nx=asize[1] & ny=asize[2]

           if keyword_set(TV) then display, im, /reuse, $
             title='Input comp image'
           
;         ;; We want to single out narrow, bright lines from the data to
;         ;; do our fitting on
;         wset,2
;         ssg_spec_extract, im, hdr, spec, xdisp, slicer=slicer_in, showplot=showplots, /TOTAL
;         ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, slicer=slicer_in, /AVERAGE
;         back_im = im
;         background = median(med_spec)
;         peak_idx = where(back_im gt background*5, count)
;         if count gt 0 then back_im[peak_idx] = background
;         im = im - back_im
           
;        display, ssg_slicer(im, hdr, slicer=slicer, /EXTRACT) ;, /reuse

           if keyword_set(showplots) then begin
              wset,winnum
              ssg_spec_extract, im, hdr, slicer=slicer_in, showplots=showplots, /TOTAL
           endif

           asize = size(slicer_in)
           if asize(0) eq 0 then $
             slicer_in = [slicer_in] ; Needs to be an array
           asize = size(slicer_in)
           npxd = asize[1]
           npd = 0
           if asize[0] gt 1 then npd = asize[2]

           ;; Collapse for speed
           orig_im = im

           trimrows = ny mod blocking
           trimlow = round(trimrows/2.)
           trimhigh = ny - trimlow
           im = im[*,trimlow:trimhigh-1]
           ny = (ny-trimrows)/blocking - 1
           smallim = fltarr(nx,ny)
           for ix = 0, nx-1 do begin
              for iy = 0,ny-1 do begin
                 smallim[ix,iy] = $
                   total(im[ix,iy*blocking:(iy+1)*blocking-1],/NAN)
              endfor
           endfor
           im = smallim

           ;; TESTING
;        im = ssg_slicer(im, hdr, slicer=0.5, /DISTORT)

;        display, ssg_slicer(im, hdr, slicer=slicer, /EXTRACT), /reuse

           if keyword_set(noninteractive) then begin
              to_pass = { image:im, hdr:hdr, slicer_size:[npxd, npd], $
                        noninteractive:noninteractive }
           endif else begin
              message, 'Hit the S key to skip this fit.  Depress and hold the D key to display images of each fitting iteration.',/CONTINUE
              to_pass = { image:im, hdr:hdr, slicer_size:[npxd, npd] }
           endelse

           slicer = tnmin('slicer_compare', slicer_in, $
                          FUNCTARGS=to_pass, /AUTODERIVATIVE, $
                          /MAXIMIZE)

           ;; Need to adjust the coefs in the cross-dispersion direction

           if npd eq 0 then begin
              slicer = slicer / blocking
              slicers[0:npxd-1, i] = slicer[*]
           endif else begin
              slicer[*,0] = slicer[*,0] / blocking
              slicers[0:npxd*npd-1, i] = slicer[*]
           endelse

           if keyword_set(TV) then begin
              ;; Re-expand
              ny = ny*blocking + trimrows
              im = rebin(im, nx, ny-trimrows)
              display, ssg_slicer(im, hdr, slicer=slicer, /EXTRACT), $
                /reuse, title='Best fit slicer image'
           endif

           ngood = ngood + 1
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
     if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  endif ;; not reviewing



  if NOT keyword_set(noninteractive) then begin
     ;; --> I am not sure if this is correct as far as 
     marked_ndays = ssg_mark_bad(ndays, rotate(slicers,4), $
                                 title=string('Slicer shape coefs in ', indir), $
                                 xtickunits='Hours', $
                                 xtitle=string('UT time (Hours) ', utdate), $
                                 ytitle='Slicer_shape_coef', $
                                 window=7, /MJD)

     dbclose

     bad_idx = where(finite(marked_ndays) eq 0, count)

     if count gt 0 then badarray[bad_idx] = badarray[bad_idx] OR 16384

     if NOT keyword_set(write) then begin
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'Write these values to the database?([Y]/N)'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'Y'
           answer = strupcase(answer)
        endrep until answer eq 'Y' or answer eq 'N'
        for ki = 0,1000 do flush_input = get_kbrd(0)
        if answer eq 'Y' then write=1
     endif

  endif ;; interactive


  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'm_slice', slicers
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated slicer parameters in ' + dbname
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end
