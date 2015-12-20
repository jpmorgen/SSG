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
pro ssg_data2max
  init = {tok_sysvar}
  ;; What I sent to Max Wed Aug 22 11:09:11 2012  jpmorgen@snipe
  ;; dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'
  ;; o14=dbfind("err_intensity<10", dbfind("intensity>0.001", dbfind("lambda=6300",dbfind("obj_code=1"))))
  ;; 
  ;; dbext, o14, 'date, nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mdate, mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc
  ;; dbext, o14, 'nrows, numlines, deldot, deldot_m, err_deldot_m', mnrows, mnumlines, mdeldot, mdeldot_m, merr_deldot_m
  ;; dbext, o14, 'phi', mphi
  ;; 
  ;; 
  ;; date = strsplit(mdate, '-', /extract)
  ;; 
  ;; for i=0,1 do print, format='(f11.4, f14.4, f16.6, f15.6, a16, a16, a16)', mlong_3[i], mphi[i], mintensity[i], merr_intensity[i], date[i]
  ;; 
  ;; for i=0,N_elements(mphi)-1 do print, format='(f11.4, f14.4, f16.6, f15.6, a16, a16, a16)', mlong_3[i], mphi[i], mintensity[i], merr_intensity[i], date[i]

  adbname = 'io_oi_analyze'
  dbopen, adbname, 0
  oentries = dbfind("err_intensity<10", dbfind("redchisq<10", dbfind("obj_code=1")))
  ;; --> beware of airglow line if I relax DOWL criterion
  lentries = dbfind("nn_DOWL<-50", oentries)
  hentries = dbfind("nn_DOWL>50", oentries)
  aentries = [lentries, hentries]
  dbext, aentries, 'date, nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', dates, ndays, long_3s, intensities, err_intensities, fconts, err_fconts, wcs, err_wcs
  dbext, aentries, 'weq, err_weq, ip', weqs, err_weqs, ips
  
  dbext, aentries, 'nrows, numlines, deldot, deldot_m, err_deldot_m', nrows, numlines, deldot, deldot_m, err_deldot_m
  dbext, aentries, 'phi', phis
  dbclose

  dates = strsplit(dates, '-', /extract)
  
  min_ews = err_fconts/fconts*wcs

  ;; The finite is not necessary, since the dbfind on redchisq
  ;; automatically takes out NaNs
  good_idx = where(finite(intensities) and weqs gt min_ews , N_pts)

  
  sort_idx = sort(ndays[good_idx])
  good_idx = good_idx[sort_idx]
  
  plot, ndays[good_idx], intensities[good_idx], psym=!tok.square, xrange=[3200,3205]
  oploterror, ndays[good_idx], intensities[good_idx], err_intensities[good_idx]
  print, N_elements(ndays), N_pts
  print,  minmax(err_intensities[good_idx])

  
  print, $
'system III	 Io phase	brightness (kR) error (kR)	   year	            month            day'
  ;;for i=0,1 do print, format='(f11.4, f14.4, f16.6, f15.6, a16, a16, a16)', long_3s[i], phis[i], intensities[i], err_intensities[i], dates[i]

  ;;for i=0,N_pts-1 do print, format='(f11.4, f14.4, f16.6, f15.6, a16, a16, a16)', long_3s[good_idx[i]], phis[good_idx[i]], intensities[good_idx[i]], err_intensities[good_idx[i]], dates[good_idx[i]]

end
