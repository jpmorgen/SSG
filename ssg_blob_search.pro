;+
; NAME: ssg_blob_search
;
; PURPOSE: Search for blobs in the Io [OI] signal
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
; ;; based on
; $Id: ssg_blip_search.pro,v 1.8 2015/03/04 15:50:32 jpmorgen Exp $
;
; $Log: ssg_blip_search.pro,v $
; Revision 1.8  2015/03/04 15:50:32  jpmorgen
; Summary: Last checkin before git
;
; Revision 1.7  2014/11/04 17:49:07  jpmorgen
; About to keep track of UT of blips to enable construction of a periodogram
;
; Revision 1.6  2013/04/29 16:39:47  jpmorgen
; About to add negative blips
;
; Revision 1.5  2013/01/31 13:39:20  jpmorgen
; About to add time limit
;
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
pro ssg_blob_search, $
   positive=positive, $
   negative=negative, $
   sigma=sigma, $
   threshold=threshold, $
   nday_threshold=nday_threshold, $
   min_cadence=min_cadence, $
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
   _EXTRA=extra ; args to plot or first_peak_find

  init = {tok_sysvar}

  if keyword_set(positive) + keyword_set(negative) ne 1 then $
     message, 'ERROR: specify /positive or /negative for the kind of blips you are looking for'

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
  ;; points for that night
  if N_elements(nday_threshold) eq 0 then $
     nday_threshold = 2

  ;; Minimum cadence in minutes.  A cadence greater than this
  ;; doesn't adequately sample the data to find blips as we are
  ;; thinking of them in this context.
  if N_elements(min_cadence) eq 0 then $
     min_cadence = 30

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
     ;; Find time differences between adjacent points
     nday_diff = mnday[idx[1:N_nday-1]] - mnday[idx[0:N_nday-2]]

     ;; Set up our loop for continuous blocks of time.
     
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
        ;; Check to see if the sampling in this gap is close enough to
        ;; our desired minimum cadence (in minutes)
        cadence = mean(nday_diff[0:idx[N_in_gap-2]]) * 24.*60.
        if cadence gt min_cadence then begin
           message, /INFORMATIONAL, 'NOTE: skipping gap which has cadence of ' + strtrim(cadence, 2) + ' minutes'
           CONTINUE
        endif
        
        if keyword_set(plot) then begin
           oplot, mnday[idx], mfcont[idx]/5, psym=!tok.triangle, thick=3
           oploterr, mnday[idx], mfcont[idx]/5, merr_fcont/5, !tok.triangle
           oplot, mnday[idx], mintensity[idx], psym=!tok.plus, thick=3
           oploterr, mnday[idx], mintensity[idx], merr_intensity, !tok.plus
           wait, 1
        endif ;; plot

        ;; For 2015 MOP see if I can find individual blobs using
        ;; first_peak_find
        blob_pos = 'None' ;; reinitialize for each segment
        ;; Index into idx which will move along from peak to peak
        last_blob_idx = 0
        sign = 1.
        if keyword_set(negative) then $
           sign = -1.
        ;; Subtract minimum in this segment to get better contrast
        mintensity[idx] = mintensity[idx] - min(mintensity[idx])
        ;; Use consistent maximum intensity for each peak in this
        ;; segment.  We have to do this because the code below
        ;; erodes mintensity as it goes along.
        max_mintensity = max(mintensity[idx])
        repeat begin
           ;; Get the position in index space of the (next) blob in
           ;; mintensity
           this_blob_pos = $
              first_peak_find( $
              sign * mintensity[idx[last_blob_idx:N_in_gap-1]], $
              'left', $
              yerr=merr_intensity[idx[last_blob_idx:N_in_gap-1]], $
              max_y=max_mintensity, $
              left_idx=left_idx, right_idx=right_idx, $
              _EXTRA=extra)
           ;; Make sure this_blob_pos it is a float
           this_blob_pos = float(this_blob_pos)
           ;; Make all idx reference to beginning of the index into
           ;; this gap idx (still need an additional unwrap to idx,
           ;; done below) Note that last_blob_idx should be integer
           this_blob_pos += last_blob_idx
           left_idx += last_blob_idx
           right_idx += last_blob_idx
           ;; Check to see if we have moved beyond the last peak
           ;; (which should not be at 0) or if we are about to run out
           ;; of points.
           done = (floor(this_blob_pos) eq last_blob_idx and $
                   last_blob_idx ne 0) $
                  or N_in_gap - last_blob_idx lt 3

           ;; Move our last_blob_idx INTEGER forward past our
           ;; floating point blob position
           last_blob_idx = min([floor(this_blob_pos+1), N_in_gap-1])
           ;; Collect valid blob positions, which are floats
           ;; referenced to idx of this segment and their statistical
           ;; significances.  
           if NOT done and $
              this_blob_pos ne 0 and $
              this_blob_pos ne (N_in_gap-1) then begin
              ;; Here is where we make sure blob_pos is referenced to
              ;; idx
              pfo_array_append, blob_pos, this_blob_pos+idx[0]
              ;; --> calculate significance
           endif
        endrep until done
        units = 'sigma'

        count = N_elements(blob_pos) 
        if count gt 0 and size(blob_pos, /type) ne !tok.string then begin
           ;; Collect precise mnday and long_3 values
           these_mnday = interpol(mnday[idx], idx, blob_pos)
           these_mlong_3 = interpol(mlong_3[idx], idx, blob_pos)
           if keyword_set(blip_plot) then begin
              ymax = max([mfcont/5, mintensity])
              plot, mnday, mfcont/5, psym=!tok.triangle, yrange=[0, ymax]
              oploterr, mnday, mfcont/5, merr_fcont/5, !tok.triangle
              oplot, mnday, mintensity, psym=!tok.plus
              oploterr, mnday, mintensity, merr_intensity, !tok.plus
              wait,1
           endif
           if keyword_set(plot) or keyword_set(blip_plot) then begin
              for iblob=0, count-1 do begin
                 plots, replicate(these_mnday[iblob], 2), !y.crange, thick=1, linestyle=!tok.dashed
              endfor
              wait,2
           endif ;; plot
           pfo_array_append, ndays, these_mnday
           pfo_array_append, long_3s, these_mlong_3
        endif 

        

        ;; Keep track of our parent distribution in sysIII.  Be careful
        ;; here, since we cannot sampling the first and last point of
        ;; each day (-->and eventually each continuous interval) for
        ;; blips,
        pfo_array_append, parent_long_3s, mlong_3[idx[1:N_in_gap-2]]
        ;; Accumulate all of our mndays
        pfo_array_append, parent_mndays, mnday

;;     plot, mnday, mintensity, psym=!tok.plus, yrange=[-20,20]
;;     oplot, mnday[1:N_nday-2], diff2, psym=!tok.diamond
;;     wait,0.2

        ;; Move the left side of our gap forward if we have any more
        ;; gaps to process
        if igap lt ntime_segments then $
           gap_left_idx = gap_right_idx[igap] + 1
     endfor ;; time segments

  endfor ;; each nday

  dbclose

  if N_elements(binsize) eq 0 then $
     binsize = 40.

  ;; Make X-axis plot in the middle of the bins
  nbins = 360./binsize
  xaxis = findgen(nbins)*binsize + binsize/2.

  ;; Here is our answer.  Catch the case where we have no blips detected
  hist = 0.
  if size(long_3s, /TNAME) ne 'STRING' then $
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
        xrange=[0,360], xtickinterval=90, xminor=9, $
        xtitle='!6System III longitude (degrees)', $
        yrange=yrange, $
        ytitle='Normalized number points', $
        $;;xmargin=[14,0], $ ;; looked good for landscape
        xmargin=[7,2], $
        _EXTRA=extra
  oploterr, xaxis, yaxis, yerr, !tok.dot

  ;;  ;; Put on threashold value
  ;;  xyouts, /norm, 0.75, 0.85, 'Threshold = ' + strtrim(threshold,2) + ' ' + units

  ;; Put in positive/negative label
  if keyword_set(positive) then $
     xyouts, /norm, 0.15, 0.85, 'Positive Departures'
  if keyword_set(negative) then $
     xyouts, /norm, 0.15, 0.85, 'Negative Departures'

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
  ;; are only sampling "up" (or down) blips
  if keyword_set(sigma) then begin
     yaxis = erfc(threshold) / 2. * mean(float(long_3_hist))
     plots, [0,360], yaxis, linestyle=!tok.dashed
     toprint = string(format='("Expected number for threshold = ", F4.1, " sigma")',  + threshold)
     ;; Put label slightly above line
     above = (!y.crange[1] - !y.crange[0]) / 30.
     xyouts, 5, yaxis[0]+above, toprint
     ;; print value as well
     print, string(format='("Expected number for threshold = ", F4.1, " sigma = ", F4.1)',  + threshold, yaxis)
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

  ;; Create a time line that marks the positions of the blips
  blipy = parent_mndays*0.
  tidx = where(ndays eq parent_mndays)
  for i=0,N_elements(ndays)-1 do begin
     idx = where(ndays[i] eq parent_mndays, count)
     if count gt 0 then $
        pfo_array_append, blipy_idx, idx
  endfor
  nblips = N_elements(blipy_idx)
  if nblips eq 0 then begin
     message, /CONTINUE, 'WARNING: no blips found'
     return
  endif
  print, 'Number of blips found: ', nblips

  ;; If we made it here, we have some points on which to do a periodogram
  blipy[blipy_idx] = 1
  ;; Do a periodogram with the ndays array, which records when the
  ;; blips occur
  scargle, parent_mndays/24., blipy, omega, psd

wset,0
   ;; plot frequency, rather than angular frequency.
   plot, omega/(2.*!pi), psd*(2.*!pi), $
         xtitle='Frequency (hr!u-1!n)', $
         ytitle='Power Spectral density', $
         xrange=[0,0.6], xstyle=!tok.exact

wset, 1
   plot, (2.*!pi)/omega, (2.*!pi)*psd, $
         xtitle='Period (hr)', $
         ytitle='Power Spectral density', $
         xrange=[0,12], xstyle=!tok.exact, $
         _EXTRA=extra


  !P.thick     = oPthick    
  !P.charsize  = oPcharsize 
  !P.charthick = oPcharthick
  !X.thick     = oXthick    
  !Y.thick     = oYthick    

end
