; $Id: ssg_display_params.pro,v 1.2 2015/03/04 16:02:03 jpmorgen Exp $

pro ssg_display_params, params, parinfo, perrors
  if N_elements(perrors) ne N_elements(params) then begin
     perrors = dblarr(N_elements(params))
 endif
  message, 'Parameter values', /CONTINUE
  format = '(a28, a20, a8, a9, a12, a20)'
  print, string(format=format, "Parameter", "Value   ", "Fixed", "Limited", "Limits", "Tied") ;, "MPPRINT"
  format = '("P(", i3, ") ", a22, f11.4, " +/- ", f7.4, i4, "  [", i1, ",", i1, "]  ", ' + $
    '" [", f9.4, ",", f9.4, "] ", a)'
  for ip = 0, N_elements(params)-1 do begin
     print, string(format=format, $
                   ip, $
                   parinfo[ip].parname	 , $
                   params [ip]     	 , $
                   perrors[ip]     	 , $
                   parinfo[ip].fixed	 , $
                   parinfo[ip].limited[0], $
                   parinfo[ip].limited[1], $
                   parinfo[ip].limits [0], $
                   parinfo[ip].limits [1], $
                   parinfo[ip].tied)        
  endfor ;; Displaying parameters

  return

end
