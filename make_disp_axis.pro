;+
; $Id: make_disp_axis.pro,v 1.1 2002/12/16 13:44:07 jpmorgen Exp $

;; Make a potentially sparse axis using a vector dispersion
;; descirption [ref wavelength, higher order dispersion terms]
;; in_axis is assumed to be an array of ordinal pixel number (but can
;; be sparse).  Ref_pixel is the pixel number (if in_axis were
;; complete) of the 0 point in the polynomial calculations
function make_disp_axis, disp, in_axis, ref_pixel
  axis = in_axis
  axis = axis * 0.
  order = N_elements(disp)-1
  for di = 0,order do begin
     axis = axis + disp[di]*(in_axis-ref_pixel)^di
  endfor
  return, axis
end

