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
; $Id: ssg_blip_search.pro,v 1.4 2012/11/27 21:57:56 jpmorgen Exp $
;
; $Log: ssg_blip_search.pro,v $
; Revision 1.4  2012/11/27 21:57:56  jpmorgen
; Fixed a bunch of stuff.  About to switch to portrait PS output
;
; Revision 1.3  2012/11/26 22:36:29  jpmorgen
; Version with bug fixed
;
; Revision 1.2  2012/11/26 22:36:10  jpmorgen
; Version I submitted for abstract
;
; Revision 1.1  2012/07/20 01:44:31  jpmorgen
; Initial revision
;
;-
pro ssg_blip_search, $
   sigma=sigma, $
   threshold=threshold, $
   nday_threshold=nday_threshold, $
   ndays=ndays, $
   long_3s=long_3s, $
   binsize=binsize, $
   east=east, $
   west=west, $
   front=front, $
   back=back, $
   plot=plot, $
   blip_plot=blip_plot, $
   ps=ps, $
   jpeg=jpeg, $
   _EXTRA=extra ; args to plot

  init = {tok_sysvar}

  ;; Be polite, but get the line thicknesses we need for PS output
  ;; (leave for terminal output since that gets us started on PS output)
  oPthick     = !P.thick
  oPcharsize  = !P.charsize
  oPcharthick = !P.charthick
  oXthick     = !X.thick
  oYthick     = !Y.thick

  !P.thick = 3
  !P.charsize = 1.5
  !P.charthick = 2
  !X.thick = 2
  !Y.thick = 2      

  ;; Default threshold for finding blips
  if N_elements(threshold) eq 0 then $
     threshold = 5
  
  ;; Default threshold for time gaps relative to the mean time between
  ;; points for tha tnight
  if N_elements(nday_threshold) eq 0 then $
     nday_threshold = 2

  ;; Initialize postscipt output
  if keyword_set(ps) then begin
     if size(ps, /TNAME) ne 'STRING' then $
       ps = 'ssg_blip_search_out.eps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /encap
  endif

  ;; Initialize output arrays for pfo_array_append
  ndays = 'None'
  long_3s = 'None'


  dbclose ;; just in case
  dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'

  ;; Find all the Io [OI] measurements
  Io_OI = dbfind(/silent, "intensity>0.001", $             ;; screen out junk
                 dbfind(/silent, "lambda=6300", $          ;; make sure we have the right line
                        dbfind(/silent, "obj_code=1")))    ;; Io

  print, 'Total number of Io [OI] points: ', N_elements(Io_OI)

  if keyword_set(east) + keyword_set(west) gt 1 then $
     message, 'ERROR: you cannot specify both east and west'

  if keyword_set(east) then begin
     Io_OI = dbfind(/silent, "side=east", Io_OI)
     print, 'Number of points on the east: ',  N_elements(Io_OI)
  endif

  if keyword_set(west) then begin
     Io_OI = dbfind(/silent, "side=west", Io_OI)
     print, 'Number of points on the west: ',  N_elements(Io_OI)
  endif

  if keyword_set(front) + keyword_set(back) gt 1 then $
     message, 'ERROR: you cannot specify both front and back'

  if keyword_set(back) then begin
     left = dbfind(/silent, "phi<90", Io_OI)
     right = dbfind(/silent, "phi>270", Io_OI)
     Io_OI = [left, right]
     print, 'Number of points in front: ',  N_elements(Io_OI)
  endif
  
  if keyword_set(front) then begin
     Io_OI = dbfind(/silent, "phi<270", $
                    dbfind(/silent, "phi>90"))
     print, 'Number of points in back: ',  N_elements(Io_OI)
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

     N_nday = N_elements(mnday)
     idx = indgen(N_nday)
     ;; Set up our loop for continuous blocks of time.
     nday_diff = mnday[idx[1:N_nday-1]] - mnday[idx[0:N_nday-2]]
     ;; Generally we have a constant cadance with occational large
     ;; pauses.  Use median instead of the mean to spot the right
     ;; side of our gaps.  This is the index into nday_diff, but it is
     ;; labeled as the gap_right_idx, since it is to be used as the
     ;; index into parent arrays
     gap_right_idx = where(nday_diff gt nday_threshold*median(nday_diff), ntime_segments)

     if ntime_segments eq 0 then begin
        ;; If no gaps, replace where's -1 with the right bound of the
        ;; array
        gap_right_idx = N_nday-1
     endif else begin
        ;; If gaps, append the right bound of the array to our gap
        ;; list since where won't find that
        pfo_array_append, gap_right_idx, N_nday-1
     endelse
     ntime_segments += 1

     ;; Put the left side of our first interval at idx=0
     gap_left_idx = 0
     for igap=0,ntime_segments-1 do begin

        ;; Plot basic data to search for correlations between intensity
        ;; and blips
        if keyword_set(plot) then begin
           plot, mnday, mfcont/5, psym=!tok.triangle
           oploterr, mnday, mfcont/5, merr_fcont/5, !tok.triangle
           oplot, mnday, mintensity, psym=!tok.plus
           oploterr, mnday, mintensity, merr_intensity, !tok.plus
           wait, 0.5
        endif ;; plot

        ;; Regenerate idx for this particular time segment
        N_in_gap = gap_right_idx[igap] - gap_left_idx + 1

        ;; Don't bother for gaps that have less than 3 points
        if N_in_gap lt 3 then begin
           message, /INFORMATIONAL, 'NOTE: skipping gap with only ' + strtrim(N_in_gap, 2) + ' points'
           CONTINUE
        endif

        idx = indgen(N_in_gap) + gap_left_idx
        print, 'new segment: ', idx

        if keyword_set(plot) then begin
           oplot, mnday[idx], mfcont[idx]/5, psym=!tok.triangle, thick=3
           oploterr, mnday[idx], mfcont[idx]/5, merr_fcont/5, !tok.triangle
           oplot, mnday[idx], mintensity[idx], psym=!tok.plus, thick=3
           oploterr, mnday[idx], mintensity[idx], merr_intensity, !tok.plus
           wait, 1
        endif ;; plot

        ;; Run a basic blip search algorithm.  Diff2 gets large if there
        ;; is a blip.
        if NOT keyword_set(sigma) then begin
           ;; Just plain intensity diffs
           diff1 = (mintensity[idx[1:N_in_gap-1]] - mintensity[idx[0:N_in_gap-2]])
           units = 'kR'
           diff2 = diff1[idx[0:N_in_gap-3]] - diff1[idx[1:N_in_gap-2]]
        endif else begin
           ;; Intensity diffs based on normalized sigma.  Had a bug in
           ;; this when I did my initial abstract.
           diff1 = (mintensity[idx[1:N_in_gap-1]] / merr_intensity[idx[1:N_in_gap-1]] $
                    - mintensity[idx[0:N_in_gap-2]] / merr_intensity[idx[0:N_in_gap-2]])
           diff2 = diff1[idx[0:N_in_gap-3]] - diff1[idx[1:N_in_gap-2]]
           units = 'sigma'

           ;; Worked this out on paper.  Just do three points at a
           ;; time + divide the cumulative difference by the average
           ;; error
           diff2 = fltarr(N_in_gap-2)
           for i=0,N_in_gap-3 do begin
              diff2[i] = (2*mintensity[idx[i+1]] - mintensity[idx[i+1]] - mintensity[idx[i+2]]) / $
                         mean(merr_intensity[idx[i:i+2]])
           endfor
        endelse

        ;; If diff2 is greater than the threshold value, add this nday to
        ;; the list of candidate ndays to check.  Note that diff2 is
        ;; shifted by a full idx value from mnday et al.  Make the
        ;; threshold read as the average distance about the continuum, so
        ;; we need to multiply it by 2
        blip_idx = where(diff2 ge threshold*2, count)
        if count gt 0 then begin
           ;; unwrap
           blip_idx = idx[blip_idx]
           ;; Plot only days with blips
           if keyword_set(blip_plot) then begin
              ymax = max([mfcont/5, mintensity])
              plot, mnday, mfcont/5, psym=!tok.triangle, yrange=[0, ymax]
              oploterr, mnday, mfcont/5, merr_fcont/5, !tok.triangle
              oplot, mnday, mintensity, psym=!tok.plus
              oploterr, mnday, mintensity, merr_intensity, !tok.plus
              wait,1
           endif
           if keyword_set(plot) or keyword_set(blip_plot) then begin
              for iblip=0, count-1 do begin
                 plots, replicate(mnday[blip_idx[iblip]+1], 2), !y.crange, thick=1, linestyle=!tok.dashed
              endfor
              wait,2
           endif ;; plot
           pfo_array_append, ndays, mnday[blip_idx+1]
           pfo_array_append, long_3s, mlong_3[blip_idx+1]
        endif

        ;; Keep track of our parent distribution in sysIII.  Be careful
        ;; here, since we cannot sampling the first and last point of
        ;; each day (-->and eventually each continuous interval) for
        ;; blips,
        pfo_array_append, parent_long_3s, mlong_3[idx[1:N_in_gap-2]]

;;     plot, mnday, mintensity, psym=!tok.plus, yrange=[-20,20]
;;     oplot, mnday[1:N_nday-2], diff2, psym=!tok.diamond
;;     wait,0.2

        ;; Move the left side of our gap forward if we have any more
        ;; gaps to process
        if igap lt ntime_segments then $
           gap_left_idx = gap_right_idx[igap] + 1
     endfor ;; time segments

  endfor

  dbclose

  if N_elements(binsize) eq 0 then $
     binsize = 40.

  ;; Make X-axis plot in the middle of the bins
  nbins = 360./binsize
  xaxis = findgen(nbins)*binsize + binsize/2.

  ;; Here is our answer
  hist = histogram(long_3s, binsize=binsize, min=0., max=359.99)
  long_3_hist = histogram(parent_long_3s, binsize=binsize, min=0., max=359.99)
print, hist
print, long_3_hist
  ;; Express bins in counts so I can see what is really there
  norm = mean(float(long_3_hist)) / float(long_3_hist)
;print, norm
;norm = 1.
  yaxis = float(hist) * norm
  yerr = sqrt(hist) * norm
  ;; Put full bin on
  pxaxis = [0, xaxis, 360]
  pyaxis = [yaxis[0], yaxis, yaxis[nbins-1]]
  yrange = [0, max(yaxis) + max(yerr)]
  plot, pxaxis, pyaxis, psym=!tok.hist, xstyle=!tok.exact, ystyle=!tok.exact+!tok.extend, $
        xrange=[0,360], xtickinterval=90, $
        xtitle='!6System III longitude (degrees)', $
        yrange=yrange, $
        ytitle='Normalized number points', $
        xmargin=[14,0], $
        _EXTRA=extra
  oploterr, xaxis, yaxis, yerr, !tok.dot

  ;;  ;; Put on threashold value
  ;;  xyouts, /norm, 0.75, 0.85, 'Threshold = ' + strtrim(threshold,2) + ' ' + units

  ;; Put on East/West / front/back labels
  if keyword_set(east) then $
     xyouts, /norm, 0.75, 0.85, 'East'
  if keyword_set(west) then $
     xyouts, /norm, 0.75, 0.85, 'West'

  ;; Put on East/West / front/back labels
  if keyword_set(front) then $
     xyouts, /norm, 0.75, 0.85, 'Anti-Jovian'
  if keyword_set(back) then $
     xyouts, /norm, 0.75, 0.85, 'Sub-Jovian'

  print, 'Average number of points that are "blips" above threashold ' + strtrim(threshold,2) + ' ' + units + ' = ' +  strtrim(mean(yaxis), 2)

  ;; Plot a dashed line of the expected statistical value for our
  ;; threshold (in sigma case).  Make sure we divide by 2, since we
  ;; are only sampling "up" blips
  if keyword_set(sigma) then begin
     yaxis = erfc(threshold) / 2. * mean(float(long_3_hist))
     plots, [0,360], yaxis, linestyle=!tok.dashed
     toprint = string(format='("Expected number for threshold = ", F4.1, " sigma (const. [OI] signal)")',  + threshold)
     xyouts, 5, yaxis[0]+0.2, toprint
  endif

  ;; Put in plane crossings
  plots, [112, 112], !y.crange, linestyle=!tok.dotted
  plots, [292, 292], !y.crange, linestyle=!tok.dotted
     
  if keyword_set(ps) then begin
     device,/close
     set_plot, 'x' 
  endif

  if keyword_set(jpeg) then begin
     if size(jpeg, /TNAME) ne 'STRING' then $
       jpeg = 'ssg_blip_search_out.jpeg'
     write_jpeg, jpeg, tvrd(true=1), quality=75, true=1
  endif

  !P.thick     = oPthick    
  !P.charsize  = oPcharsize 
  !P.charthick = oPcharthick
  !X.thick     = oXthick    
  !Y.thick     = oYthick    


end
