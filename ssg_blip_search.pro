;+
; NAME: ssg_blip_search
;
; PURPOSE: Search for blips in the Io [OI] signal
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
; $Id: ssg_blip_search.pro,v 1.1 2012/07/20 01:44:31 jpmorgen Exp $
;
; $Log: ssg_blip_search.pro,v $
; Revision 1.1  2012/07/20 01:44:31  jpmorgen
; Initial revision
;
;-
pro ssg_blip_search, $
   sigma=sigma, $
   threshold=threshold, $
   ndays=ndays, $
   long_3s=long_3s, $
   binsize=binsize, $
   east=east, $
   west=west, $
   _EXTRA=extra ; args to plot

  init = {tok_sysvar}

  ;; Default threshold for finding blips
  if N_elements(threshold) eq 0 then $
     threshold = 5

  ndays = 'None'
  long_3s = 'None'


  dbclose ;; just in case
  dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'

  ;; Find all the Io [OI] measurements
  Io_OI = dbfind(/silent, "intensity>0.001", $             ;; screen out junk
                 dbfind(/silent, "lambda=6300", $          ;; make sure we have the right line
                        dbfind(/silent, "obj_code=1")))    ;; Io

  if keyword_set(east) + keyword_set(west) gt 1 then $
     message, 'ERROR: you cannot specify both east and west'

  if keyword_set(east) then begin
     Io_OI = dbfind(/silent, "side=east", Io_OI)
  endif

  if keyword_set(west) then begin
     Io_OI = dbfind(/silent, "side=west", Io_OI)
  endif

  ;; Handle each nday one at a time
  for inday=0,4000 do begin
     ;; Create the strings necessary to query the ZDBASE for nday
     ndayl = string(format='("nday>", i5)', inday)
     ndayh = string(format='("nday<", i5)', inday+1)
     OI = dbfind(/silent, ndayl, $ ;; correct nday
                 dbfind(/silent, ndayh, Io_OI), $
                 count=count)

     ;; Don't bother with ndays unless they have at least 3 points
     if count lt 3 then $
        CONTINUE

     ;; Extract quantities we care about.
     dbext, OI, 'nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc
     dbext, OI, 'nrows, numlines, deldot, deldot_m, err_deldot_m', mnrows, mnumlines, mdeldot, mdeldot_m, merr_deldot_m
     dbext, OI, 'phi', mphi

;;     ;; Plot basic data to search for correlations between intensity
;;     ;; and blips
;;     plot, mnday, mfcont/5, psym=!tok.triangle
;;     oploterr, mnday, mfcont/5, merr_fcont/5, !tok.triangle
;;     oplot, mnday, mintensity, psym=!tok.plus
;;     oploterr, mnday, mintensity, merr_intensity, !tok.plus

     N_nday = N_elements(mnday)
     idx = indgen(N_nday)
     ;; Set up our loop for continuous blocks of time.
     nday_diff = mnday[idx[1:N_nday-1]] - mnday[idx[0:N_nday-2]]
     gap_idx = where(nday_diff gt 2*mean(nday_diff), ntime_segments)
     ntime_segments += 1
     ;; Set up our default interval idx
     gap_idx_left = 0
     ;; Handle the case where we have no gap
     if gap_idx eq !tok.nowhere then $
        gap_idx = idx[N_nday-1]
     for igap=0,ntime_segments-1 do begin
        gap

        ;; Run a basic blip search algorithm.  Diff2 gets large if there
        ;; is a blip.
        if keyword_set(sigma) then begin
           ;; Intensity diffs based on normalized sigma
           diff1 = (mintensity[idx[1:N_nday-1]] / merr_intensity[idx[1:N_nday-1]] $
                    - mintensity[idx[0:N_nday-2]] / merr_intensity[idx[1:N_nday-1]])
           units = 'sigma'
        endif else begin
           ;; Just plain intensity diffs
           diff1 = (mintensity[idx[1:N_nday-1]] - mintensity[idx[0:N_nday-2]])
           units = 'kR'
        endelse
        diff2 = diff1[idx[0:N_nday-3]] - diff1[idx[1:N_nday-2]]

        ;; If diff2 is greater than the threshold value, add this nday to
        ;; the list of candidate ndays to check.  Note that diff2 is
        ;; shifted by a full idx value from mnday et al.  Make the
        ;; threshold read as the average distance about the continuum, so
        ;; we need to multiply it by 2
        blip_idx = where(diff2 ge threshold*2, count)
        if count gt 0 then begin
           if bad_nday_count gt 0 then begin
              message, /CONTINUE, 'WARNING: NOT adding blips on nday= ' + strtrim(mnday[0], 2) + ' with discontinuous time'
              CONTINUE

           endif
           pfo_array_append, ndays, mnday[blip_idx+1]
           pfo_array_append, long_3s, mlong_3[blip_idx+1]
        endif

        ;; Keep track of our parent distribution in sysIII.  Be careful
        ;; here, since we cannot sampling the first and last point of
        ;; each day (-->and eventually each continuous interval) for
        ;; blips,
        pfo_array_append, parent_long_3s, mlong_3[1:N_nday-2]

;;     plot, mnday, mintensity, psym=!tok.plus, yrange=[-20,20]
;;     oplot, mnday[1:N_nday-2], diff2, psym=!tok.diamond
;;     wait,0.2

     endfor ;; time segments

  endfor

  dbclose

  if N_elements(binsize) eq 0 then $
     binsize = 30.
  hist = histogram(long_3s, binsize=binsize)
  long_3_hist = histogram(parent_long_3s, binsize=binsize)

  xaxis = findgen(360./binsize)*binsize
  yaxis = float(hist)/float(long_3_hist)
  plot, xaxis, yaxis, psym=!tok.hist, xstyle=!tok.extend+!tok.exact, ystyle=!tok.extend, $
        xrange=[0,360], $
        xtitle='!6System III longitude (degrees)', $
        ytitle='Fraction of points that are "blips" above threashold = ' + strtrim(threshold,2) + ' ' + units, $
        _EXTRA=extra
  oploterr, xaxis, yaxis, sqrt(hist)/float(long_3_hist), !tok.dot

  print, 'Average fraction of points that are "blips" above threashold ' + strtrim(threshold,2) + ' ' + units + ' = ' +  strtrim(mean(yaxis), 2)

  ;; Plot a dashed line of the expected statistical value for our
  ;; threshold (in sigma case).  Make sure we divide by 2, since we
  ;; are only sampling "up" blips
  if keyword_set(sigma) then begin
     yaxis = erfc(threshold) / 2.
     plots, [0,360], yaxis, linestyle=!tok.dashed
     xyouts, 0, yaxis[0]+0.002, 'Expected Fraction for ' + strtrim(threshold, 2) + ' sigma', $
             charsize=3
  endif

  ;; Put in plane crossings
  plots, [112, 112], !y.crange, linestyle=!tok.dotted
  plots, [292, 292], !y.crange, linestyle=!tok.dotted
     
;;  ;; Put in east/west annotation
;;  if keyword_set(east) then $
;;     xyouts


end
