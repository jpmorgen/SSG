;+
; $Id: where_nday_eq.pro,v 1.2 2002/12/16 13:41:30 jpmorgen Exp $

; where_nday_eq: returns the item number(s) in the currently open
; database whose nday values are within 1E-e5 JD (1.157 s) within the
; input nday.  The input nday can be vector.  Basically a wrapper
; around dbfind that deals with real numbers in the specific case of
; nday, where you know the necessary tolerance.


;-

function where_nday_eq,in_nday,listin,SILENT=silent,fullstring = Fullstring,      $
        errmsg=errmsg, count=count, tolerance=tolerance

  ON_ERROR, 2
  npts = N_elements(in_nday)
  if npts eq 0 then return, -1
  nday = dblarr(npts)
  nday[*] = in_nday[*]
  if NOT keyword_set(tolerance) then tolerance = 0.00001
  if N_elements(tolerance) eq 1 then $
    tolerance = replicate(tolerance, npts)
  if N_elements(tolerance) ne npts then $
    message, 'ERROR: tolerance should either by a single number or an array with the same number of elements as nday'
  ;; The database stuff does not handle multiple calls quite the way
  ;; the IDL model suggests, so let's do it by hand here

  for i=0,npts-1 do begin
      findstring=string(format='("nday = ", f11.5, "(", f11.5, ")")', $
                        nday[i], tolerance[i])
      if n_params() gt 1 then begin
          entry = dbfind(findstring[i],listin,SILENT=silent, $
                         fullstring = Fullstring, errmsg=errmsg, Count = count)
      endif else begin
          entry = dbfind(findstring,SILENT=silent, $
                         fullstring = Fullstring, errmsg=errmsg, Count = count)
      endelse
      if count ne 0 then begin
          if N_elements(entries) eq 0 then $
            entries = [entry] $
          else $
            entries = [entries, entry]
      endif
  endfor
  
  count = N_elements(entries)
  if count eq 0 then entries = -1
  return, entries
  
end
