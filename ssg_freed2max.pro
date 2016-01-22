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
pro ssg_freed2max
  init = {tok_sysvar}
  dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'
  o14=dbfind("err_intensity<10", dbfind("intensity>0.001", dbfind("lambda=6300",dbfind("obj_code=1"))))

  dbext, o14, 'date, nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mdate, mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc
  dbext, o14, 'nrows, numlines, deldot, deldot_m, err_deldot_m', mnrows, mnumlines, mdeldot, mdeldot_m, merr_deldot_m
  dbext, o14, 'phi, time', mphi, mtime


  date = strsplit(mdate, '-', /extract)

  ;;for i=0,1 do print, format='(f11.4, f14.4, f16.6, f15.6, a6, a6, a6)', mlong_3[i], mphi[i], mintensity[i], merr_intensity[i], date[i]

  for i=0,N_elements(mphi)-1 do print, format='(f11.4, f14.4, f16.6, f15.6, a6, a6, a6, a12)', mlong_3[i], mphi[i], mintensity[i], merr_intensity[i], date[i], mtime[i]

  dbclose
end
