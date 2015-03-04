;+
; $Id: make_disp_axis.pro,v 1.2 2015/03/04 15:59:10 jpmorgen Exp $

;; Make a potentially sparse axis using a vector dispersion
;; descirption [ref wavelength, higher order dispersion terms]
;; in_axis is assumed to be an array of ordinal pixel number (but can
;; be sparse).  Ref_pixel is the pixel number (if in_axis were
;; complete) of the 0 point in the polynomial calculations
function make_disp_axis, indisp, in_axis, ref_pixel, scaled=scaled
  disp = indisp
  if keyword_set(scaled) then begin
     for i=0,N_elements(disp)-1 do begin
        disp[i] = disp[i] ;; /10^(3.*i)
     endfor
  endif
  return, poly(in_axis-ref_pixel, disp)
end

