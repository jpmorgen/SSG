;+
; $Id: ssg_spec_extract.pro,v 1.2 2002/12/16 13:42:12 jpmorgen Exp $

; ssg_spec_extract.  Extract dispersion and cross-dispersion spectra
; from a SSG file or image/header pair.  Automatically de-rotates
; camera rotation if CAM_ROT keyword is present in header.

;-

pro ssg_spec_extract, im_or_fname, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, sigma_cut=sigma_cut, showplots=showplots, sli_cent=sli_cent, center=center, cam_rot=cam_rot, slicer=slicer, blocking=blocking, average=av, total=tot, title=title

  ON_ERROR, 2
  if N_elements(im_or_fname) eq 0 then $
    message, 'ERROR: no filename or image supplied'
  if size(im_or_fname, /TNAME) eq 'STRING' then $
    im=ssgread(im_or_fname, hdr, /DATA) $ ; make sure orientation is correct
  else im = im_or_fname
  if N_elements(size(im, /DIMENSIONS)) ne 2 then $
    message, 'ERROR: specify a valid filename or 2D array'

  if NOT keyword_set(av) and NOT keyword_set(tot) then $
    message, 'ERROR: specify /AVERAGE or /TOTAL to determine how the average and median spectra will be returned: e.g. multiplying or dividing by the total number of good pixels'

  asize=size(im) & nx=asize[1] & ny=asize[2]
  spec = fltarr(nx)
  xdisp = fltarr(ny)
  med_spec = fltarr(nx)
  med_xdisp = fltarr(ny)
  ;; Start with everything NAN and only put in values if they
  ;; are good.
  spec[*] = !values.f_nan
  xdisp[*] = !values.f_nan
  med_spec[*] = !values.f_nan
  med_xdisp[*] = !values.f_nan
  ;; Normally we want to have the image pivot in place when derotated
  ;; so as to avoid any unnecessary translations, but we could choose
  ;; to center it if we wanted to
  PIVOT = 1
  if keyword_set(center) then PIVOT = 0
  if N_elements(cam_rot) eq 0 then $
    cam_rot = sxpar(hdr, 'CAM_ROT') ; 0 if not present
  if keyword_set(sli_cent) eq 0 then $
    sli_cent = sxpar(hdr, 'SLI_CENT') ; 0 if not present
  if sli_cent eq 0 and cam_rot ne 0 then begin
     message, /CONTINUE, 'WARNING: you want to rotate the image, but have not specified a center (these things should be in the FITS header).  For now I will assume center of image is the center of rotation.'
     sli_cent = ny/2.
  endif
  
  ;; Camera rotation stuff is easy.  I am not sure if this saves time,
  ;; but might as well check
  if cam_rot ne 0 then $
    im = rot(im, -cam_rot, 1., nx/2., sli_cent, PIVOT=PIVOT, $
            cubic=-0.5, missing = !values.f_nan)


  ;; Slicer shape is more subtle, since some stretching can occur
  im = ssg_slicer(im, hdr, slicer=slicer, blocking=blocking, /EXTRACT)

  ;; Dispersion 
  for si=0,nx-1 do begin
     good_idx = where(finite(im[si,*]), count)
     if count gt 0 then begin
        if keyword_set(tot) then begin
           spec[si] = total(im[si,good_idx])
           med_spec[si] = median(im[si,good_idx])*count
        endif else begin
           spec[si] = total(im[si,good_idx])/count
           med_spec[si] = median(im[si,good_idx])
        endelse
     endif
  endfor

  ;; Cross-dispersion
  for di=0,ny-1 do begin
     good_idx = where(finite(im[*,di]), count)
     if count gt 0 then begin
        if keyword_set(tot) then begin
           xdisp[di] = total(im[good_idx,di])
           med_xdisp[di] = median(im[*,di])*count
        endif else begin
           xdisp[di] = total(im[good_idx,di])/count
           med_xdisp[di] = median(im[*,di])
        endelse
     endif
  endfor

  if NOT keyword_set(showplots) then return

  plus = 1
  asterisk = 2
  dot = 3
  diamond = 4
  triangle = 5
  square = 6
  psym_x = 7

  solid=0
  dotted=1
  dashed=2
  dash_dot=3
  dash_3dot = 4
  long_dash=5

  ;;  This can get a lot fancier with the image, etc.

  bunit = sxpar(hdr, 'BUNIT', count=count)
  if count eq 0 then bunit = 'DN'

  !p.multi=[0,2]
  plot,spec, linestyle=solid, $
       title=title, $
       xtitle='Pixel (dispersion direction)', $
       ytitle=string('Value (', bunit, '), average solid, median dotted'), $
       yrange=[min([spec,med_spec],/NAN), max([spec,med_spec],/NAN)], $
       ystyle=2
  oplot, med_spec, linestyle=dotted

  plot,xdisp, linestyle=solid, $
       title=title, $
       xtitle='Pixel (cross-dispersion direction)', $
       ytitle=string('Value (', bunit, '), average solid, median dotted'), $
       yrange=[min([xdisp,med_xdisp],/NAN), max([xdisp,med_xdisp],/NAN)], $
       ystyle=2
              
  oplot, med_xdisp, linestyle=dotted
  !p.multi=0
  
end
