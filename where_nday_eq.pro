;+
; $Id: where_nday_eq.pro,v 1.1 2002/10/15 20:42:22 jpmorgen Exp $

; where_nday_eq: returns the item number(s) in the currently open
; database whose nday values are within 1E-e5 JD (1.157 s) within the
; input nday.  The input nday can be vector.  Basically a wrapper
; around dbfind that deals with real numbers in the specific case of
; nday, where you know the necessary tolerance.


;-

function where_nday_eq,nday,listin,SILENT=silent,fullstring = Fullstring,      $
        errmsg=errmsg, Count = count

  findstring=string(format='("nday = ", f11.5, "(0.00001)")', nday)
  if n_params() gt 1 then $
    return, dbfind(findstring,listin,SILENT=silent,fullstring = Fullstring,    $
        errmsg=errmsg, Count = count)

  return, dbfind(findstring,SILENT=silent,fullstring = Fullstring,             $
        errmsg=errmsg, Count = count)


end
