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
   nday_threshold, $ ;; minimum number of ndays between segments we want to median
   stdev=stdev

  init = {tok_sysvar}
  init = {ssg_sysvar}
  color
  tek_color

  
  if N_elements(nday_threshold) eq 0 then $
     nday_threshold = 180
  
  ;;adbname = '/data/io/ssg/analysis/archive/database/2015-09-19_to_max/io_oi_analyze'
  adbname = 'io_oi_analyze'
  dbopen, adbname, 0
  oentries = dbfind("obj_code=1", dbfind("redchisq<5"))
  aentries = oentries
  lentries = dbfind("nn_DOWL<-50", oentries)
  hentries = dbfind("nn_DOWL>50", oentries)
  aentries = [lentries, hentries]
  dbext, aentries, "nday, weq, err_weq, ip, intensity, err_intensity, alf, delta, wc, err_wc", ndays, weqs, err_weqs, ips, intensities, err_intensities, alfs, deltas, wcs, err_wcs
  dbext, aentries, "nn_DOWL, nn_ew, nn_Dw, nn_Lw, redchisq", nn_DOWLs, nn_ews, nn_Dws, nn_Lws, redchisqs
  dbclose

  print, 'median reduced chi^2: ', median(redchisqs)

  sort_idx = sort(ndays)
  uts = ndays[sort_idx] + (julday(1,1,1990,0))
  plot, uts, intensities[sort_idx], psym=!tok.dot, $
        xtickunits = ['Years'], $
        xtitle='!6Year', $
        ytitle='Intensity (kR)', $
        ystyle=!tok.no_box, xmargin=[12,8]
  axis, /yaxis, ytitle='Model scale factor x10!U15!N'

  proc_label = 'median'
  if keyword_set(stdev) then $
     proc_label = 'stdev'
  al_legend, ['Machine data point', 'Hand data point', 'Scale factor (nightly)', 'Machine ' + proc_label, 'Hand ' + proc_label, 'Scale ' + proc_label], $
             psym=[!tok.dot, !tok.dot, !tok.dot, !tok.diamond, !tok.triangle, !tok.square], $
             colors=['Opposite', 'red', 'blue', 'Opposite', 'Opposite', 'Opposite', 'Opposite']

  ;; Also include Melanie's database and Max's fits
  oplots = 0
  repeat begin
     oplots += 1

     ;; Loop through larger groups of ndays to find medians for those
     ;; groups.  This is patterened after ssg_blip_search and
     ;; ssg_blob_search
     N_nday = N_elements(ndays)
     ;; Find time differences between adjacent ndays.  Note that this
     ;; will index into sort_idx
     nday_diff = ndays[sort_idx[1:N_nday-1]] - ndays[sort_idx[0:N_nday-2]]
     gap_right_idx = where(nday_diff gt nday_threshold, ntime_segments)
     ;; Put on the far right index
     gap_right_idx = [gap_right_idx, N_nday-1]

     gap_left_idx = 0
     for igap=0,ntime_segments do begin
        right = gap_right_idx[igap]
        to_plot = median(intensities[sort_idx[gap_left_idx:right]])
        if keyword_set(stdev) then $
           to_plot = stddev(intensities[sort_idx[gap_left_idx:right]], /NAN)
        oplot, [mean(uts[gap_left_idx:right])], $
               [to_plot], $
               psym=!tok.diamond+oplots-1
        gap_left_idx = right+1
     endfor

     case oplots of
        1: begin
           ;; Read Melanie's database
           mdb = 'io6300_integrated'
           dbopen, mdb
           entries=dbfind("err_intensity<10", $
                          dbfind("intensity>0.001", $
                                 dbfind("lambda=6300", $
                                        dbfind("obj_code=1"))), count=N_m)
           dbext, entries, 'nday, LONG_3, phi, intensity, err_intensity', ndays, mLONG_3s, mphis, intensities, err_intensities
           dbclose
           ;; --> see if I can get a better display this way
           ;;ndays += 180
        end
        2: begin
           ;; Read Max's scale factors and convert to ndays an
           ;; fake intensities so we can use the code above
           restore, '/data/io/ssg/analysis/max/scalefacs.sav'
           sf = read_ascii('/data/io/ssg/analysis/max/scalefacs.dat', template=scalefacs_template)

           ndays = julday(sf.month, sf.day, sf.year) - !ssg.JDnday0
           intensities = sf.scale

           ;;sort_idx = sort(ndays)
           ;;uts = ndays[sort_idx] + (julday(1,1,1990,0))
           ;;plot, uts, intensities[sort_idx], psym=!tok.asterisk, $
           ;;      xtickunits = ['Months', 'Years'], ymargin=[12,2], $
           ;;      xtitle='!6Year', xrange=julday(1,1,[1997,2000]), $
           ;;      ytitle='Model scale factor x10!U15!N'
        end
        else:
     endcase

     sort_idx = sort(ndays)
     uts = ndays[sort_idx] + (julday(1,1,1990,0))
     oplot, uts, intensities[sort_idx], psym=!tok.dot, color=!tok.red-1+oplots

  endrep until oplots eq 3

end
