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
pro ssg_median, $
   nday_threshold ;; minimum number of ndays between segments we want to median

  init = {tok_sysvar}
  
  if N_elements(nday_threshold) eq 0 then $
     nday_threshold = 7
  
  adbname = '/data/io/ssg/analysis/archive/database/2015-09-19_to_max/io_oi_analyze'
  dbopen, adbname, 0
  oentries = dbfind("obj_code=1", dbfind("redchisq<5"))
  aentries = oentries
  lentries = dbfind("nn_DOWL<-50", oentries)
  hentries = dbfind("nn_DOWL>50", oentries)
  aentries = [lentries, hentries]
  dbext, aentries, "nday, weq, err_weq, ip, intensity, err_intensity, alf, delta, wc, err_wc", ndays, weqs, err_weqs, ips, intensities, err_intensities, alfs, deltas, wcs, err_wcs
  dbext, aentries, "nn_DOWL, nn_ew, nn_Dw, nn_Lw", nn_DOWLs, nn_ews, nn_Dws, nn_Lws
  dbclose

  sort_idx = sort(ndays)
  uts = ndays[sort_idx] + (julday(1,1,1990,0))
  plot, uts, intensities[sort_idx], psym=!tok.dot, $
        xtickunits = ['Years'], $
        xtitle='!6Year', $
        ytitle='Intensity (kR)'

  ;; Loop through larger groups of ndays to find medians for those
  ;; groups.  This is patterened after ssg_blip_search and
  ;; ssg_blob_search
  N_nday = N_elements(ndays)
  ;; Find time differences between adjacent ndays.  Note that this
  ;; will index into sort_idx
  nday_diff = ndays[sort_idx[1:N_nday-1]] - ndays[sort_idx[0:N_nday-2]]
  gap_right_idx = where(nday_diff gt nday_threshold, ntime_segments)
  ;; Put on the far right index
  gap_right_idx = [gap_right_idx, N_nday]

  gap_left_idx = 0
  for igap=0,ntime_segments do begin
     right = gap_right_idx[igap]-1
     oplot, [mean(uts[gap_left_idx:right])], $
            [median(intensities[sort_idx[gap_left_idx:right]])], $
            psym=!tok.square
     gap_left_idx = right+1
  endfor
end
