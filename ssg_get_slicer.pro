;+
; $Id: ssg_get_slicer.pro,v 1.2 2002/11/21 20:04:23 jpmorgen Exp $

; ssg_get_slicer.  derive slicer shape parameters and record them in
; the database

;-

function slicer_compare, in_slicer, image_or_dp, image=im_or_fname, hdr=hdr, slicer_size=slicer_size, blocking=blocking, cr_cutval=cr_cutval

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
    im=readfits(im_or_fname, hdr) $
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

  answer = strupcase(get_kbrd(0))
  if answer eq 'D' then begin
     display, im, /reuse
     display, ref_im, /reuse
  endif
  if answer eq 'S' then message, 'STOPPING FIT'
  return, total(im*ref_im, /NAN)
end

pro ssg_get_slicer, indir, VERBOSE=verbose, TV=tv, zoom=zoom, slicer=slicer_in, blocking=blocking, flat_cut=flat_cut, cr_cutval=cr_cutval, showplots=showplots

;  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0
  if NOT keyword_set(blocking) then blocking = 10
  if NOT keyword_set(cr_cutval) then cr_cutval = 10

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  ;; Get all the files in the directory so we can mark camrot as not
  ;; measured on the ones where we can't measure it.
  entries = dbfind(string("dir=", indir))

  dbext, entries, "fname, typecode, m_slice, bad", files, typecodes, slicers, badarray
  files=strtrim(files)
  nf = N_elements(files)

  slicers[*] = !values.f_nan
  
  if NOT keyword_set(slicer_in) then slicer_in=fltarr(1)
  if keyword_set(showplots) then begin
     window,2, title='Raw comp spectra'
     window,3, title='Cleaned comp spectra'
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

        im = ssgread(files[i], hdr, /DATA)
        flatfile = strtrim(sxpar(hdr,'FLATFILE',COUNT=count))
        if count eq 0 then message, 'WARNING: works better if you call ssg_flatfield first', /CONTINUE

        cam_rot = strtrim(sxpar(hdr,'CAM_ROT',COUNT=count))
        if count eq 0 then message, 'ERROR: file '+ files[i] + ' does not have a CAM_ROT keyword.  You need to run ssg_[get&fit]_camrot stuff first'
        im = rot(im, -cam_rot, cubic=-0.5)
        sxaddpar, hdr, 'CAM_ROT', 0, 'Derotated temporarily by ssg_get_slicer'

        flat=ssgread(flatfile, fhdr)
        cam_rot = strtrim(sxpar(fhdr,'CAM_ROT',COUNT=count))
        if count eq 0 then message, 'ERROR: file '+ flatfile + ' does not have a CAM_ROT keyword.  You need to run ssg_[get&fit]_camrot stuff first'
        flat = rot(flat, -cam_rot, cubic=-0.5)

        if N_elements(flat_cut) eq 0 then begin
           flat_cut = sxpar(fhdr, 'FLAT_CUT', count=count)
           if count eq 0 then message, 'ERROR: no FLAT_CUT found in flatfield file.  You must therefore specify flat_cut (value below which flatfield image is not divided) on the command line.  0.75 should work OK'
        endif

        ;; Remove cosmic ray hits
        ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, slicer=slicer, /AVERAGE
        template = template_create(im, med_spec, xdisp)
        template = ssg_slicer(im, hdr, slicer=slicer, /DISTORT)
        sigma_im = template_statistic(im, template)
        mask_im = mark_bad_pix(sigma_im, cutval=cr_cutval)
        badidx = where(mask_im gt 0, count)
        if count gt 0 then im[badidx] = !values.f_nan

        ;; Mask out bad flatfield regions
        flat = normalize(flat, flat_cut)

        bad_idx = where(flat le flat_cut, count)
        if count gt 0 then begin
           im[bad_idx] = !values.f_nan
           message, /CONTINUE, 'Setting unflattened pixels of ' + files[i] + ' to NAN'
        endif

        ;; Make a smaller image that includes just good rows.
        asize=size(im) & nx=asize[1] & ny=asize[2]
        firsty = 0
        repeat begin
           temp = where(finite(im[*,firsty]), count)
           if count eq 0 then firsty = firsty + 1
        endrep until count ne 0
        lasty = ny-1
        repeat begin
           temp = where(finite(im[*,lasty]), count)
           if count eq 0 then lasty = lasty - 1
        endrep until count ne 0

        im = im[*,firsty:lasty]
        asize=size(im) & nx=asize[1] & ny=asize[2]

        if keyword_set(TV) then display, im, /reuse
        
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
           wset,3
           ssg_spec_extract, im, hdr, slicer=slicer_in, showplots=showplots, /TOTAL
        endif

        asize = size(slicer_in)
        if asize(0) eq 0 then $
          slicer_in = [slicer_in] ; Needs to be an array
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

        message, 'Hit the S key to skip this fit.  Depress and hold the D key to display images of each fitting iteration.',/CONTINUE
        to_pass = { image:im, hdr:hdr, slicer_size:[npxd, npd] }
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
           display, ssg_slicer(im, hdr, slicer=slicer, /EXTRACT), /reuse
        endif

        ngood = ngood + 1
     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  dbclose

  if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'm_slice', slicers
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated slicer parameters in ' + dbname

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end
