;+
; $Id: nday2date.pro,v 1.1 2015/02/17 23:00:05 jpmorgen Exp $

; nday2date  Converts nday into a string of the form
; yyyy-mm-ddThh:mm:ss
; If keyword /dir supplied, nday (array) converted to array of
; directory names

;; Works for arrays of nday too.

;-

function nday2date, $
   nday, $
   dir=dir

  init = {ssg_sysvar}

  N_ndays = N_elements(nday)
  if N_ndays eq 0 then $
     message, 'ERROR: specify one or more ndays'

  jd = nday + !ssg.JDnday0
  caldat, jd, month, day, year, hour, minute, second
  for i=0,N_ndays-1 do begin
     tmp = string(format='(I4.4, 2("-", I2.2), "T", 3(I2.2, :, ":"))', year[i], month[i], day[i], hour[i], minute[i], second[i])
     if keyword_set(date) then $
       date = [date, tmp] $
     else $
       date = tmp
  endfor
  if NOT keyword_set(dir) then $
     return, date

  ;; If we made it here, we are constructing directory names.  Check
  ;; if we have an array of ndays.  If so, get ready to construct a
  ;; list of unique directory names
  u_idx = lindgen(N_ndays)
  fnday = floor(nday)
  if N_ndays gt 1 then begin
     sort_idx = sort(nday)
     u_idx = uniq(fnday[sort_idx])
     u_idx = sort_idx[u_idx]     
  endif ;; list of ndays to compress
  ;; Make our directory name
  for i=0,N_elements(u_idx)-1 do begin
     UT = strsplit(nday2date(fnday[u_idx[i]]), 'T', /extract)
     ymd = strsplit(UT[0], '-', /extract)
     ;; Top level through year is easy
     dir = !ssg.top + path_sep() + 'reduced' + path_sep() + $
           ymd[0] +  path_sep()
     ;; Handle the Y2K issue
     y = ymd[0]
     if float(y) lt 2000 then $
        y = strmid(y,2)
     dir += y + string(format='(2(I02))', ymd[1:2])
     dirs = array_append(dir, dirs)
  endfor ;; each unique integer nday
  return, dirs
end
