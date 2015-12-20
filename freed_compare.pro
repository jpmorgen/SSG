;+
; NAME: freed_compare
;
; PURPOSE: Compare to Melanie Freed's database
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
pro freed_compare
  init = {ssg_sysvar}
  init = {tok_sysvar}

  mdb = 'io6300_integrated'
  dbopen, mdb
  entries=dbfind("err_intensity<10", $
                 dbfind("intensity>0.001", $
                        dbfind("lambda=6300", $
                               dbfind("obj_code=1"))), count=N_m)
  dbext, entries, 'nday, LONG_3, phi, intensity, err_intensity', mndays, mLONG_3s, mphis, mintensities, merr_intensities
  dbclose

  ;;adbname = 'io_oi_analyze'
  adbname = '/data/io/ssg/analysis/archive/database/2015-09-19_to_max/io_oi_analyze'
  dbopen, adbname, 0

  for im=0, N_m-1 do begin
     ;; Find matches between Melanie's database and
     ;; io_oi_analyzes to 10 minute accuracy
     aentry = where_nday_eq(mndays[im], tolerance=10d/60d/24d, count=count)
     if count eq 0 then begin
        message, /CONTINUE, 'NOTE: Marking ' + string(format='(f12.4)', mndays[im]) + ' to be removed from Melanie''s list'
        mndays[im] = !values.f_NAN
        CONTINUE
     endif
     ;; If we made it here, we have
     aentries = array_append(aentry, aentries)
  endfor
  good_idx = where(finite(mndays))
  mndays = mndays[good_idx]
  mintensities = mintensities[good_idx]
  merr_intensities = merr_intensities[good_idx]
  mLONG_3s = mLONG_3s[good_idx]
  mphis = mphis[good_idx]

  dbext, aentries, "nday, long_3, phi, weq, err_weq, ip, intensity, err_intensity", ndays, long_3s, phis, weqs, err_weqs, ips, intensities, err_intensities
  dbclose

  delta = mintensities - intensities
  errs = sqrt(merr_intensities^2 + err_intensities^2)
  ;;plot, mndays, delta, psym=!tok.dot
  plot, delta, psym=!tok.dot
  ;;errplot, mndays, delta + errs/2., delta - errs/2.
  errplot, delta + errs/2., delta - errs/2.

  ;;oplot, mndays, mintensities-30, psym=!tok.dot
  ;;plot, ndays, intensities, psym=!tok.dot
  ;;plot, mndays, mintensities, psym=!tok.dot

  plot, mintensities, intensities, psym=!tok.square
  ;;oploterror, mintensities, $
  ;;            intensities, $
  ;;            merr_intensities, $
  ;;            err_intensities
  xaxis = indgen(30)
  oplot, xaxis, xaxis

  ;;plot, merr_intensities, err_intensities, psym=!tok.square
  ;;oplot, xaxis, xaxis

  plot, long_3s-mlong_3s, psym=!tok.square
  plot, phis-mphis, psym=!tok.square

  stop

  sigmas = delta / errs
  plot, sigmas, psym=!tok.dot

  binsize = 0.1
  min = -10
  max = 10
  h = histogram(sigmas, binsize=binsize, min=-10, max=10)
  plot, indgen((max - min)/binsize)*binsize - max, h

  good_idx = where(finite(intensities), complement=bad_idx, N_bad)
  print, ndays[bad_idx]

  stop  
end
