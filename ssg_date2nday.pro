;+
; NAME:
;
; PURPOSE:
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id:$
;
; $Log:$
;-
function ssg_date2nday, date, checking=checking
  init = {ssg_sysvar}
  init = {eph_sysvar}

  ;; Don't raise error when checking
  if NOT keyword_set(checking) then begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'ERROR: date format probably not yyyy-mm-ddTHH:MM:SS, where separators can be space or / as well as - and time after T is optional. ' + strtrim(date, 2)
     endif ;; error
  endif ;; regular run

  ;; Works with standard format of yyyy-mm-ddTHH:MM:SS and also / and
  ;; space 
  date_arr = float(strsplit(date, /extract, /regex, ' |-|/|T|:'))
  
  ;; reduced Julian day
  juldate, date_arr[0:2], rawjd
  rawjd = rawjd + !eph.jd_reduced
  nday = rawjd - !ssg.JDnday0
  ;; Check to make sure we get the same thing back, but don't
  ;;do recursive forever!
  if NOT keyword_set(checking) then begin
     date_check = nday2date(nday)
     nday_check = ssg_date2nday(date_check, /checking)
     if nday_check ne nday then $
        message, 'ERROR: date format probably not yyyy-mm-ddTHH:MM:SS, where separators can be space or / as well as - and time is optional. ' + strtrim(date, 2)
  endif ;; checking

  return, nday

end
