;+
; $Id: nday2date.pro,v 1.1 2015/02/17 23:00:05 jpmorgen Exp $

; nday2date  Converts nday into a string of the form
; yyyy-mm-ddThh:mm:ss

;; Works for arrays of nday too.

;-

function nday2date, nday

  init = {ssg_sysvar}

  jd = nday + !ssg.JDnday0
  caldat, jd, month, day, year, hour, minute, second
  for i=0,N_elements(nday)-1 do begin
     tmp = string(format='(I4.4, 2("-", I2.2), "T", 3(I2.2, :, ":"))', year[i], month[i], day[i], hour[i], minute[i], second[i])
     if keyword_set(date) then $
       date = [date, tmp] $
     else $
       date = tmp
  endfor
  return, date
end
