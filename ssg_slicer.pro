;+
; $Id: ssg_slicer.pro,v 1.3 2003/03/10 18:32:42 jpmorgen Exp $

; ssg_slicer.  morph image to reflect distortions induced by slicer
; rotation and lens aberration.  

;-

function ssg_slicer, im_or_fname, hdr, slicer=in_slicer, blocking=blocking, extract=extract, distort=distort, delete=delete

;  ON_ERROR, 2
  if N_elements(im_or_fname) eq 0 then $
    message, 'ERROR: no filename or image supplied'
  if size(im_or_fname, /TNAME) eq 'STRING' then $
    im=ssgread(im_or_fname, hdr, eim, ehdr, /DATA) $
  else im = im_or_fname
  if N_elements(size(im, /DIMENSIONS)) ne 2 then $
    message, 'ERROR: specify a valid filename or 2D array'

  if NOT keyword_set(extract) and NOT keyword_set(distort) then $
    message, 'ERROR: specify /EXTRACT (undo effect of slicer--as in for extracting a spectrum) or /DISTORT (put the effect of the slicer in--for use with templates)'

  ;; Extract slicer info from header (if available), otherwise just
  ;; return the original uncorrected array
  if N_elements(in_slicer) eq 0 then begin
     if N_elements(hdr) eq 0 then return, im
     npxd=0
     repeat begin
        temp = sxpar(hdr, $
                     string(format='("SLICER0", i1)', npxd), count=count)
        npxd = npxd + 1
     endrep until count eq 0
     npxd = npxd - 1
     if npxd eq 0 then return, im

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
           for ipd = 0, npd - 1 do begin
              in_slicer[ipxd, ipd] = $
                sxpar(hdr, string(format='("SLICER", i1, i1)', ipd, ipxd))
           endfor
        endfor
     endelse
  endif

  slicer = in_slicer
  if N_elements(slicer) eq 0 then return, im

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
  sli_cent = sxpar(hdr, 'SLI_CENT', count=count)
  if count eq 0 then sli_cent = nx/2.

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

  ;; The slicer shape, which is expressed as a polynomial fit in
  ;; the Y-direction varies slowly as a function of dispersion
  ;; direction.  This code creates a map of dispersion distortions
  ;; for each row
  for iy = 0,ny-1 do begin
     trow[*] = im[*,iy]
     ;; x and y are now in pixel number referenced to the center of
     ;; the slicer pattern
     x = indgen(nx) - nx/2.
     y = iy - sli_cent
     ;; dx is the array of dispersion direction offsets 
     dx = fltarr(nx)

     ;; This is a little weird, but in the case of dispersion
     ;; direction modifications, we need a fresh set of coeficients
     ;; for each x
     if npd gt 0 then begin
        coefi = fltarr(nx)
     endif

     ;; The general scheme is to replace each row in the image with a
     ;; row that is properly resampled using IDL's interpol routine.
     ;; The first (maybe only) row in the slicer coefficient array
     ;; describes how the rows are shifted relative to the center
     ;; pixel of the image.  The subsequent rows in the slicer array
     ;; give the perturbation of the initial coefficient as a function
     ;; of dispersion direction.

     ;; For each cross-dispersion direction coefficient
     for ipxd=0,npxd-1 do begin
        if npd eq 0 then begin
           coefi = slicer[ipxd]
           dx = dx + coefi * y^(ipxd+1)
        endif else begin
           for ipd=0, npd-1 do begin
              coefi = coefi + slicer[ipxd,ipd] * x^(ipd)
           endfor               ; ipd
        endelse
        ;; 0th order is always 0, so just skip it entirely.  note that
        ;; this works for both the 1D and vector versions of coefi
        dx = dx + coefi * y^(ipxd+1)
     endfor                     ; ipxd

     ;; Having accumulated all the perterbations to the mapping of the
     ;; X axis, now we are ready to do the shifting, which we run
     ;; forwards if we are doing /DISTORT and run backwards if we are
     ;; doing /EXTRACT
     if keyword_set(extract) then dx = - dx

     trow = interpol(trow, x, x + dx)
     im[*,iy] = trow[*]
  endfor                        ; iy

  ;; Re-expand if collapsed
  if keyword_set(blocking) then begin
     ny = ny*blocking + trimrows
     im = rebin(im, nx, ny-trimrows)
     orig_im[*,trimlow:trimhigh-1] = im[*,*]
     im = orig_im
  endif

  ;; Delete SLICER keywords if the /EXTRACTed image is being written 
  if keyword_set(delete) then begin
     if NOT keyword_set(extract) then $
       message, 'WARNING: you are deleting a slicer description from a header while distorting.  This is probably a bad idea, but I will do it anyway', /CONITNUE
     for ipxd = 0, npxd - 1 do begin
        for ipd = 0, npd - 1 do begin
           sxdelpar, hdr, string(format='("SLICER", i1, i1)', ipxd, ipd)
        endfor
     endfor
  endif

  return, im

end
