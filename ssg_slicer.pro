;+
; $Id: ssg_slicer.pro,v 1.1 2002/11/12 21:31:13 jpmorgen Exp $

; ssg_slicer.  morph image to reflect distortions induced by slicer
; rotation and lens aberration.  

;-

function ssg_slicer, im_or_fname, hdr, slicer=in_slicer, blocking=blocking, extract=extract, distort=distort

;  ON_ERROR, 2
  if N_elements(im_or_fname) eq 0 then $
    message, 'ERROR: no filename or image supplied'
  if size(im_or_fname, /TNAME) eq 'STRING' then $
    im=ssgread(im_or_fname, hdr, /DATA) $ ; make sure orientation is correct
  else im = im_or_fname
  if N_elements(size(im, /DIMENSIONS)) ne 2 then $
    message, 'ERROR: specify a valid filename or 2D array'

  if NOT keyword_set(extract) and NOT keyword_set(distort) then $
    message, 'ERROR: specify /EXTRACT (undo effect of slicer--as in for extracting a spectrum) or /DISTORT (put the effect of the slicer in--for use with templates)'

  ;; Extract slicer info from header (if available), otherwise just
  ;; return the original uncorrected array
  if N_elements(in_slicer) eq 0 then begin
     if N_elements(hdr) eq 0 then return, im
     temp = sxpar(hdr, 'SLICER0*', count=count)
     if count eq 0 then return, im
     npxd = count
     npd = 0
     repeat begin
        temp = sxpar(hdr, $
                     string(format='("SLICER", i1, "0")', npd), count=count)
        npd = npd + 1
     endrep until count eq 0
     npd = npd - 1
     in_slicer = fltarr(npxd)
     if npd eq 0 then begin
        in_slicer = sxpar(hdr, 'SLICER0*')
     endif else begin
        in_slicer = fltarr(npxd,npd)
        for ipxd = 0, npxd - 1 do begin
           in_slicer[ipxd, ipd] = $
             sxpar(hdr, string(format='("SLICER", i1, i1)', ipd, ipxd))
        endfor
     endelse
  endif
  slicer = in_slicer
  if N_elements(slicer) eq 0 then return, im
  if keyword_set(extract) then slicer=-slicer
  asize = size(slicer)
  if asize(0) eq 0 then $
    slicer = [slicer]           ; Needs to be an array

  asize = size(slicer)
  npxd = asize[1]               ; # of poly coef in cross-disp direction

  ;; Dispersion direction corrections, if desired
  npd = 0         
  if asize[0] gt 1 then $
    npd = asize[2]              ; # of poly coef in disp direction

  asize=size(im) & nx=asize[1] & ny=asize[2]
  ;; Collapse for speed
  if keyword_set(blocking) then begin
     orig_im = im
     orig_im[*] = !values.f_nan ; set trimmed rows to NAN (usually background anyway)
     trimrows = ny mod blocking
     trimlow = round(trimrows/2.)
     trimhigh = ny - trimlow
     im = im[*,trimlow:trimhigh-1]
     ny = (ny-trimrows)/blocking
     im = rebin(im, nx, ny)
  endif

  trow = fltarr(nx)

  ;; For each cross-dispersion element, define a dispersion axis
  ;; remapping.  Reuse x and y variables.  Reference is the center
  ;; of the image, so polynomials don't have a 0th order term
  for iy = 0,ny-1 do begin
     ;; y, the reference for the xdisp polynomial, is 0 at the
     ;; center of the image
     y = iy - ny/2.
     ;; dx0 ends up being a constant offset applied to all
     ;; dispersion-direction pixels in this particular
     ;; cross-dispersion element
     dx0 = 0.
     for ipxd=0,npxd-1 do begin
        temp = slicer[ipxd]
        if npd gt 0 then $
          temp = slicer[ipxd,0]
        ;; 0th order is always 0, so just skip it entirely
        dx0 = dx0 + temp * y^(ipxd+1)
     endfor                     ; ipxd
     ;; Now concentrate on the dispersion direction (x).  We can do
     ;; this as a whole array using interpol.  Again, the center of
     ;; the image is the origin of the polynomial, but now dx is
     ;; going to be different for each pixel
     x = indgen(nx) - nx/2.
     dx = fltarr(nx)
     if npd gt 0 then begin
        for ipd=1,npd-1 do begin
           ;; 0th order is always 0, so just skip it entirely
           dx = dx + slicer[0,ipd] * x^(ipd+1)
         endfor                  ; ipd
      endif
      trow[*] = im[*,iy]
      ;; Negative signs on dx0 and dx get the slope in y in the right
      ;; sense, though since images have y=0 on the bottom, you might
      ;; think this is backwards.  In any case, it really doesn't
      ;; matter, since this is the only place things are defined.
      trow = interpol(trow, x, x + dx0 + dx)
      im[*,iy] = trow[*]
  endfor                        ; iy


  ;; Re-expand if collapsed
  if keyword_set(blocking) then begin
     ny = ny*blocking + trimrows
     im = rebin(im, nx, ny-trimrows)
     orig_im[*,trimlow:trimhigh-1] = im[*,*]
     im = orig_im
  endif

  return, im

end
