;+
; $Id: ssg_fit.pro,v 1.1 2002/12/16 13:34:04 jpmorgen Exp $

; ssg_fit.  Use MPFITFUN to fit the Io spectra
; nday_range can just be 

;-

pro ssg_fit, nday_start_or_range

  ON_ERROR, 2
  if NOT keyword_set(nday_start_or_range) then $
    nday_start_or_range = [0,36500] ; Somewhat generous :-)

  nday_range = nday_start_or_range
  if N_elements(nday_range) eq 1 then $
    nday_range = [nday_start_or_range, 36500]

  repeat begin
     nday=ssg_select(nday_range)
     ssg_fit1spec, nday
  endrep until nday eq -1

end

