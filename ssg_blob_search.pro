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
   model_sub=model_sub, $
   positive=positive, $
   negative=negative, $
   threshold=threshold, $
   contrast=contrast, $
   nday_threshold=nday_threshold, $
   min_cadence=min_cadence, $
   ndays=ndays, $
   long_3s=long_3s, $
   phis=phis, $
   sigmas=sigmas, $
   widths=widths, $
   plot=plot, $
   blob_plot=blob_plot, $
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

  ;; Default threshold for finding blobs
  if N_elements(threshold) eq 0 then $
     threshold = 0.2

  ;; Default contrast for finding left and right idx of blobs
  if N_elements(contrast) eq 0 then $
     contrast = 0.2

  ;; Default threshold for time gaps relative to the mean time between
  ;; points for that night
  if N_elements(nday_threshold) eq 0 then $
     nday_threshold = 2

  ;; Minimum cadence in minutes.  A cadence greater than this
  ;; doesn't adequately sample the data to find blips as we are
  ;; thinking of them in this context.
  if N_elements(min_cadence) eq 0 then $
     min_cadence = 30

  if N_elements(intensity_tolerance) eq 0 then $
     intensity_tolerance = 0.001
  
  ;; Initialize postscipt output
  if keyword_set(ps) then begin
     if size(ps, /TNAME) ne 'STRING' then $
       ps = 'ssg_blip_search_out.eps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /encap
  endif

  ;; Initialize output arrays for pfo_array_append
  ndays = 'None'
  sigmas = 'None'
  widths = 'None'
  long_3s = 'None'
  phis = 'None'

  ;; Read in the model results
  if keyword_set(model_sub) then begin
     model_top = !ssg.top + path_sep() + 'analysis' + path_sep() + 'max' +  path_sep()
     restore, model_top + 'britfil.dM2915.1line_template.sav'
     model = read_ascii(model_top + 'britfil.dM2915.1line', template=model_template)
     ;; jdcnv provides answers in double precision, which have trouble
     ;; converting to integers the way I want, so tweak the hours a
     ;; little bit off of zero and then take the floor
     jdcnv, model.year, model.month, model.day, 0.1, model_jds
     model_indays = floor(model_jds - !ssg.JDnday0)
  endif

  dbclose ;; just in case
  dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'

  ;; Find all the Io [OI] measurements
  Io_OI = dbfind(/silent, "intensity>0.001", $             ;; screen out junk
                 dbfind(/silent, "lambda=6300", $          ;; make sure we have the right line
                        dbfind(/silent, "obj_code=1")))   ;; Io

  print, 'Total number of Io [OI] points: ', N_elements(Io_OI)

  ;; Handle each nday one at a time
  for inday=0,4000 do begin
  ;;for inday=3572,3572 do begin
  ;; Create the strings necessary to query the ZDBASE for nday
     ndayl = string(format='("nday>", i5)', inday)
     ndayh = string(format='("nday<", i5)', inday+1)
     OI = dbfind(/silent, ndayl, $ ;; correct nday
                 dbfind(/silent, ndayh, Io_OI), $
                 count=data_count)

     ;; Don't bother with ndays unless they have at least 3 points
     if data_count lt 3 then $
        CONTINUE

     print, 'NDAY = ', inday

     ;; Extract quantities we care about.
     dbext, OI, 'nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc
     dbext, OI, 'nrows, numlines, deldot, deldot_m, err_deldot_m', mnrows, mnumlines, mdeldot, mdeldot_m, merr_deldot_m
     dbext, OI, 'phi', mphi

     if keyword_set(model_sub) then begin
        ;; Find our overlap between the model and this nday
        model_nday_idx = where(model_indays eq inday, model_count)
        if model_count eq 0 then begin
           message, 'WARNING: no nday match found in model for nday ' + strtrim(inday, 2), /CONTINUE
           CONTINUE
        endif
        ;; Check to make sure we are really lined up
        if data_count ne model_count then begin
           if data_count lt model_count then $
              message, 'ERROR: fewer data points than model points.  Something is not right'
           message, 'WARNING: data and model mismatch.  Aligning to model.', /CONTINUE
           align_idx = 'None'
           for imodel=0, model_count-1 do begin
              this_align_idx = where(abs(model.data[model_nday_idx[imodel]] - mintensity) $
                                     le intensity_tolerance, count)
              if count eq 0 then $
                 message, 'ERROR: no data point found to match model point ' + strtrim(model.data[imodel], 2)
              pfo_array_append, align_idx, this_align_idx
           endfor
           mnday          = mnday         [align_idx]
           mlong_3        = mlong_3       [align_idx]
           mintensity     = mintensity    [align_idx]
           merr_intensity = merr_intensity[align_idx]
           mfcont         = mfcont        [align_idx]
           merr_fcont     = merr_fcont    [align_idx]
           mwc            = mwc           [align_idx]
           merr_wc        = merr_wc       [align_idx]
           mphi           = mphi          [align_idx]
        endif
        ;; Make a new variable, scaled_model, which is the raw model
        ;; multiplied by the scale factor which we will compare to the
        ;; data.  All indices into scaled_model will match those into
        ;; the database quantities we extract below
        scaled_model = model.model_scale1[model_nday_idx] * model.model_scale[model_nday_idx]
        ;; For now, just adjust mintensity when we want to do blob
        ;; searching in the model-subtracted data
        mintensity -= scaled_model
     endif

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
           ;; Move the left side of our gap forward if we have any more
           ;; gaps to process
           if igap lt ntime_segments then $
              gap_left_idx = gap_right_idx[igap] + 1
           CONTINUE
        endif

        idx = indgen(N_in_gap) + gap_left_idx
        print, 'new segment: ', idx
        ;; Check to see if the sampling in this gap is close enough to
        ;; our desired minimum cadence (in minutes)
        cadence = mean(nday_diff[gap_left_idx:gap_right_idx[igap]-1]) * 24.*60.
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
        last_left_idx = 0
        sign = 1.
        if keyword_set(negative) then $
           sign = -1.
        ;; Subtract minimum in this segment to get better contrast
        mintensity[idx] = mintensity[idx] - min(mintensity[idx])
        ;; Use consistent maximum intensity for each peak in this
        ;; segment.  We have to do this because the code below
        ;; erodes mintensity as it goes along. --> significance
        ;; calculation ends up making this not so important
        max_mintensity = max(mintensity[idx])
        repeat begin
           ;; Get the position in index space of the (next) blob in
           ;; mintensity
           this_blob_pos = $
              first_peak_find( $
              sign * mintensity[idx[last_left_idx:N_in_gap-1]], $
              'left', $
              yerr=merr_intensity[idx[last_left_idx:N_in_gap-1]], $
              threshold=threshold, $
              contrast=contrast, $
              $;;max_y=max_mintensity, $
              left_idx=left_idx, right_idx=right_idx, $
              _EXTRA=extra)
           ;; Make sure this_blob_pos it is a float
           this_blob_pos = float(this_blob_pos)
           ;; Make all idx reference to beginning of the index into
           ;; this gap idx (still need an additional unwrap to idx,
           ;; done below) Note that last_left_idx is an integer
           this_blob_pos += last_left_idx
           left_idx += last_left_idx
           right_idx += last_left_idx
           ;; Move our last_left_idx forward past this peak
           last_left_idx = right_idx
           ;; We want fully formed peaks, so check for the cases where
           ;; first_peak_find finds the first or last point in our
           ;; segment (i.e. where the first and last points are local
           ;; maxima).  It ends up being more concise to move
           ;; last_left_idx above this section and tweak it back in
           ;; this test
           if this_blob_pos ne last_left_idx-right_idx and $
              this_blob_pos ne (N_in_gap-1) then begin
              ;; Here is where we make sure the idx are referenced to
              ;; the full set of observations on this nday
              this_blob_pos += gap_left_idx
              left_idx += gap_left_idx
              right_idx += gap_left_idx
              ;; Collect valid blob positions
              pfo_array_append, blob_pos, this_blob_pos
              ;; Calculate statistical significance.  First subtract
              ;; the continuum under the peak
              m = (mintensity[right_idx] - mintensity[left_idx]) / $
                  (right_idx - left_idx)
              b = mintensity[right_idx] - m * right_idx
              width = right_idx - left_idx
              cont = m * (indgen(width) + left_idx) + b
              peak_int = total(mintensity[left_idx:right_idx] - cont)
              err_int = sqrt(total(merr_intensity[left_idx:right_idx]^2))
              pfo_array_append, sigmas, peak_int / err_int
              pfo_array_append, widths, width
           endif
        endrep until N_in_gap - last_left_idx lt 3

        count = N_elements(blob_pos) 
        if count gt 0 and size(blob_pos, /type) ne !tok.string then begin
           ;; Collect precise mnday and long_3 values
           these_mnday = interpol(mnday[idx], idx, blob_pos)
           these_mlong_3 = interpol(mlong_3[idx], idx, blob_pos)
           these_mphis = interpol(mphi[idx], idx, blob_pos)
           if keyword_set(blob_plot) then begin
              ymax = max([mfcont/5, mintensity])
              plot, mnday, mfcont/5, psym=!tok.triangle, yrange=[0, ymax]
              oploterr, mnday, mfcont/5, merr_fcont/5, !tok.triangle
              oplot, mnday, mintensity, psym=!tok.plus
              oploterr, mnday, mintensity, merr_intensity, !tok.plus
              wait,1
           endif
           if keyword_set(plot) or keyword_set(blob_plot) then begin
              for iblob=0, count-1 do begin
                 plots, replicate(these_mnday[iblob], 2), !y.crange, thick=1, linestyle=!tok.dashed
              endfor
              wait,2
           endif ;; plot
           pfo_array_append, ndays, these_mnday
           pfo_array_append, long_3s, these_mlong_3
           pfo_array_append, phis, these_mphis
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



  !P.thick     = oPthick    
  !P.charsize  = oPcharsize 
  !P.charthick = oPcharthick
  !X.thick     = oXthick    
  !Y.thick     = oYthick    

end
