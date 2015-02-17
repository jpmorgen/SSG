;+
; $Id: ssg_autofit.pro,v 1.1 2015/02/17 23:02:07 jpmorgen Exp $

; ssg_fit.  Use MPFITFUN to fit the Io spectra
; nday_range can just be 

;-

pro ssg_autofit, nday_start_or_range

;  ON_ERROR, 2
  init = {ssg_sysvar}
  if NOT keyword_set(nday_start_or_range) then $
    nday_start_or_range = [0,36500] ; Somewhat generous :-)

  nday_range = nday_start_or_range
  if N_elements(nday_range) eq 1 then $
    nday_range = [nday_start_or_range, 36500]


  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind(string("nday>", nday_range[0]), $ ; < is really <=
                   dbfind(string("nday<", nday_range[1]), $
                          dbfind("typecode=5", $
                                 dbfind("bad=0"))))
  dbext, entries, "nday, obj_code", ndays, obj_codes
  dbclose

  for i=0, N_elements(ndays)-1 do begin

     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping nday = ' + strtrim(ndays[i], 2), /CONTINUE
     endif else begin
        case obj_codes[i] of
           1 : obj = !eph.io
           2 : obj = !eph.europa
           3 : obj = !eph.ganymede
           4 : obj = !eph.callisto
           else : obj = -1
        endcase

        if obj lt 0 then $
          CONTINUE

        ssg_fit1spec, ndays[i], obj, /autofit, /quiet

     endelse ;; CATCH
     CATCH, /CANCEL

  endfor

end

