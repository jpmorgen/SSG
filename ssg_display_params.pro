; $Id: ssg_display_params.pro,v 1.1 2002/12/16 13:33:40 jpmorgen Exp $

pro ssg_display_params, params, parinfo
  message, 'Parameter values', /CONTINUE
  format = '(a28, a11, a7, a9, a12, a20)'
  print, string(format=format, "Parameter", "Value", "Fixed", "Limited", "Limits", "Tied") ;, "MPPRINT"
  format = '("P(", i3, ") ", a22, f11.4, i4, "  [", i1, ",", i1, "]  ", ' + $
    '" [", f9.4, ",", f9.4, "] ", a)'
  for ip = 0, N_elements(params)-1 do begin
     print, string(format=format, $
                   ip, $
                   parinfo[ip].parname	, $
                   params [ip]     	, $
                   parinfo[ip].fixed	, $
                   parinfo[ip].limited[0], $
                   parinfo[ip].limited[1], $
                   parinfo[ip].limits [0], $
                   parinfo[ip].limits [1], $
                   parinfo[ip].tied)        
  endfor ;; Displaying parameters

  return

end
