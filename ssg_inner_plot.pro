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
pro ssg_inner_plot, $
   wait=wait, $
   _EXTRA=extra
  init = {ssg_sysvar}
  init = {eph_sysvar}
  sep = path_sep()
  openr, lun, /get_lun,  !ssg.top + sep + 'analysis' + sep + 'max' $
         + sep + 'inner_torus_runs_set3.txt'
  line = ''
  while NOT EOF(lun) do begin
     readf, lun, line
     date = float(strsplit(line, /extract))
     print, date
     ;; reduced Julian day
     juldate, date, rawjd
     rawjd = rawjd + !eph.jd_reduced
     nday = rawjd - !ssg.JDnday0
     ssg_smyth_chi2, nday1=nday, /plot, _EXTRA=extra
     wait, wait
  endwhile
  close, lun
end
