;+
; $Id: ssg_get_sliloc.pro,v 1.8 2014/07/02 14:46:56 jpmorgen Exp jpmorgen $

; ssg_get_sliloc.  Find the top and bottom pixels (in Y) of the slicer
; pattern and the center in the image in the dispersion direction

;-

pro ssg_get_sliloc, indir, VERBOSE=verbose, TV=tv, showplots=showplots, zoom=zoom, pos=pos, write=write, noninteractive=noninteractive, review=review, window=winnum, rwindow=rwinnum, plot=plot, limits=in_limits, nflat_steps=nflat_steps, ndata_steps=ndata_steps, wait=wait, bad_flat_threshold=bad_flat_threshold

;  ON_ERROR, 2
  ;init = {ssg_sysvar}
  cd, indir

  silent = 1

  if keyword_set(verbose) then silent = 0
  if NOT keyword_set(winnum) then  winnum=6
  if NOT keyword_set(rwinnum) then rwinnum=7
  if N_elements(wait) eq 0 then $
    wait = 0

  plus = 1
  asterisk = 2
  dot = 3
  diamond = 4
  triangle = 5
  square = 6
  psym_x = 7

  solid=0
  dotted=1
  dashed=2
  dash_dot=3
  dash_3dot = 4
  long_dash=5

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  ;; Get all the files in the directory so we can mark slicer position as not
  ;; measured on the ones where we can't measure it.
  entries = dbfind(string("dir=", indir), /fullstring)

  dbext, entries, "fname, nday, date, typecode, bad, m_sli_bot, e_sli_bot, m_sli_top, e_sli_top, sli_cent, e_sli_cent", $
         files, ndays, dates, typecodes, badarray, m_sli_bots, e_sli_bots, m_sli_tops, e_sli_tops, sli_cents, e_sli_cents

  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)
  if NOT keyword_set(review) then begin ; We really want to do all the fitting

     m_sli_bots[*] = !values.f_nan
     e_sli_bots[*] = !values.f_nan
     m_sli_tops[*] = !values.f_nan
     e_sli_tops[*] = !values.f_nan
     sli_cents[*] = !values.f_nan
     e_sli_cents[*] = !values.f_nan

     ngood = 0
     err=0

     if keyword_set(showplots) then window,winnum

     ;; Read in all files so they can be marked properly in the
     ;; database.  Exclude bias and dark images.
     good_idx = where(typecodes ge 2, nf)
     flat_idx = where(typecodes[good_idx] eq 3, nflats)
     message, 'Reading all files except darks and flats', /INFORMATIONAL
     ;; Read in a file to get the size of the cross-dispersion array
     im = ssgread(files[0], hdr, eim, ehdr, /DATA, /TRIM)
     asize = size(im) & nx = asize(1) & ny = asize(2)
     ;; Save both 2nd derivative and value
     xdisps_d2 = fltarr(ny, nf)
     xdisps = xdisps_d2
     for ifile=0,nf-1 do begin
        im = ssgread(files[good_idx[ifile]], hdr, eim, ehdr, /DATA, /TRIM)
        ssg_spec_extract, im, hdr, spec, xdisp, med_xdisp=y, /total 
        ;; For comps, use the total x-disp spectrum, though the second
        ;; derivative amplitude turns out to be significantly less
        if typecodes[ifile] eq 2 then begin
           y = xdisp
        endif
        tny = N_elements(y)
        if tny ne ny then begin
           message, /CONTINUE, 'WARNING: mismatch between first file cross-dipersion size: ' + strtrim(ny, 2) + ' and current cross-dipersion size: ' + strtrim(tny, 2) + ' adjusting current size'
           new_y = fltarr(ny)
           new_y = y[0:min([ny, tny])-1]
           y = new_y
        endif

        dy = deriv(y[0:ny-1])
        d2y = deriv(dy)
        if keyword_set(showplots) then begin
           plot, d2y
           wait, wait
        endif
        xdisps_d2[*,ifile] = d2y
        xdisps[*,ifile] = y
     endfor
     ;; histeq scaling works nicely.
     ;;atv, xdisps_d2
     ;; Make the second derivative  display always 512 pixels high
     if keyword_set(TV) then $
       display, xdisps_d2, zoom=[4,512/nf], /reuse, title='Cross dispersion second derivatives'


     if nflats le 1 then $
       message, 'ERROR: there must be at least 2 flats for this algorithm to work properly.  There is unlikely to be useful data in this directory.'

     message, 'Computing correlations between flats', /INFORMATIONAL
     ;; The idea is to shift our second derivatives relative to each
     ;; other in cross dispersion space to see where they correlate
     ;; the best.  Lets make the number of steps we take general so we
     ;; can play with how far we need to go in each direction.  Right
     ;; now I just use shift, so we go in 1 pixel increments.

     ;; We work with the flats first in order to make an ueber flat
     ;; that we then run back through all of the data.  The flats have
     ;; high, narrow peaks in their second derivatives at the edges.
     ;; That means we should catch the peak correlation between flats
     ;; even if we check over a broad range (e.g. more than two
     ;; slices).  Our 10 slices fit inside of ny pixels, with some
     ;; margin.  Let the user input the nflat_steps parameter and also
     ;; nuke flats that slide over to the next slice with the
     ;; bad_flat_threshold logic below.  An extreme case is
     ;; reddir='/home/jpmorgen/data/ssg/reduced/2008/08jun10', which
     ;; benefits from ny/8. or nflat_steps = 25 or so.  Unfortunately,
     ;; the algorithm doesn't really work well for
     ;; cross-correlating between flats of such drastically different
     ;; illumination.  It is clear that secondary peaks in the
     ;; correlation are dominating
     nsteps = ny/10.
     if keyword_set(nflat_steps) then $
       nsteps = nflat_steps
     ;; When the CCD is binned, sometimes there are only 3 steps,
     ;; which is not enough for the algorithm to work well.  Try
     ;; making sure there are at least 5 steps.
     nsteps = nsteps > 5

     ;; Do the correlates only for the flats at first.
     correlates = fltarr(nsteps, nflats, nflats)
     xdisp_correlate = fltarr(nsteps)
     xdisp_peaks = fltarr(nflats, nflats)
     for ifiles_shift=0,nflats-1 do begin
        for ixdisp_shift=0, nsteps-1 do begin
           ;; A little confusing with the -ifiles shift.  Use two
           ;; computer keyboards to illustrate why you want it this way.
           shift_d2 = shift(xdisps_d2, ixdisp_shift-nsteps/2., -ifiles_shift)
           ;; This caused more problems than it solved
           ;;;; Shift wraps xdisps_d2, which is why it is fast,
           ;;;; but we don't want the signal from the wraps
           ;;if round(ixdisp_shift-nsteps/2.) lt 0 then $
           ;;  shift_d2[nsteps+round(ixdisp_shift-nsteps/2.):nsteps-1,*] = 0
           ;;if round(ixdisp_shift-nsteps/2.) gt 0 then $
           ;;  shift_d2[0:round(ixdisp_shift-nsteps/2.)-1,*] = 0
           for ifile=0, nflats-1 do begin
              correlates[ixdisp_shift,ifiles_shift,ifile] = $
                total(xdisps_d2[*,ifile] * shift_d2[*,ifile], /NAN)
           endfor
        endfor
     endfor

     message, 'Finding peaks in correlations between flats', /INFORMATIONAL
     if keyword_set(showplots) then $
       window, 2
     for ifiles_shift=0,nflats-1 do begin
        for ifile=0,nflats-1 do begin
           ;; Find peak in cross dispersion correlation.  Data seems
           ;; to have a good peak + climb back up towards secondary
           ;; peaks.  Start at the good peak and find the fist valley
           ;; on either side.
           xdisp_correlate = reform(correlates[*, ifiles_shift, ifile])
           junk = max(xdisp_correlate, peak_idx)
           right = peak_idx + $
                   first_peak_find(-xdisp_correlate[peak_idx:nsteps-1], $
                                   'left', /poly, /quiet)
           left = first_peak_find(-xdisp_correlate[0:max([0,peak_idx-1])], $
                                  'right', /poly, /quiet)
           xdisp_peaks[ifiles_shift, ifile] = $
             peak_find(xdisp_correlate[left:right], XTOL=1E5, FTOL=1E5) $
             + left - nsteps/2.
           if keyword_set(showplots) then begin
              plot, xdisp_correlate, title=string('xdisp_correlate comparing flats ', ifile, ifiles_shift)
              plots, replicate(xdisp_peaks[ifiles_shift, ifile] + nsteps/2., 2), !y.crange
              plots, replicate(left,2), !y.crange, linestyle=dashed
              plots, replicate(right,2), !y.crange, linestyle=dashed
              wait, wait
           endif ;; Plotting

        endfor 
     endfor

     ;;atv, xdisp_peaks
     ;;stop
     if keyword_set(TV) then $
       display, xdisp_peaks, zoom=10, /reuse, title='xdisp_peak matrix (flats)'

     peak_meds = fltarr(nflats)
     peak_means = fltarr(nflats)
     for ifile=0, nflats-1 do begin
        peak_meds[ifile] = median(xdisp_peaks[*, ifile])
        peak_means[ifile] = mean(xdisp_peaks[*, ifile])
        ;; Get median and mean offsets of each file from the flats
     endfor

     ;; Find flats that are outside of our threshold so we don't
     ;; include them in our ueber flat 
     if NOT keyword_set(bad_flat_threshold) then $
       bad_flat_threshold = 2
     bad_flat_idx = where(abs(peak_meds) gt bad_flat_threshold, count, $
                          Ncomplement=ngood_flats, complement=good_flat_idx)
     if ngood_flats eq 0 then $
       message, 'ERROR: cross-correlations between flats results in no flats being closer than ' + strtrim(bad_flat_threshold, 2) + ' pixels.  Consider running ssg_get_sliloc with the /PLOT option to see what is going on'

     if keyword_set(plot) then begin
        window,0, xs=640,ys=512
        plot, peak_meds, yrange=[-nsteps/2., nsteps/2.], title='Median (Mean) Peak translations (flats only, X > bad_flat_threshold)'
        oplot, peak_means, linestyle=dashed
        if count gt 0 then $
          oplot, bad_flat_idx, peak_meds[bad_flat_idx], psym=psym_x
     endif

     message, 'Creating ueberflat from ' + strtrim(ngood_flats ,2) + ' of ' + strtrim(nflats, 2) + ' flats.  Use /PLOT option to see what is going on and bad_flat_threshold keyword to tweak.', /INFORMATIONAL

     ;; Make an "ueber flat"
     xdisp_axis = indgen(ny)

     ;; Here is where we line all our flats up with each other.  
     ;; Wed Jul  2 10:48:43 2014  jpmorgen@snipe
     ;; Do this in both second derivative space and regular value space
     shifted_flats_d2 = fltarr(ny, ngood_flats)
     shifted_flats = shifted_flats_d2
     for iflat=0, ngood_flats-1 do begin
        ;; Slicers with large shifts don't appear to be lining
        ;; up well.  See e.g.:
        ;; reddir='/home/jpmorgen/data/ssg/reduced/2008/08jun10'
        ;; Check to see if the problem is with interpol or
        ;; the quality of the correlation.  Hmm.  The problem seems to
        ;; be with the quality of the correlation.  I guess it is a
        ;; slicer illumination effect.
        this_xdisp_d2 = xdisps_d2[*, flat_idx[good_flat_idx[iflat]]]
        this_xdisp = xdisps[*, flat_idx[good_flat_idx[iflat]]]
        ;; Peak_meds applies only to flats.  Don't forget to unwrap!
        to_shift = peak_meds[good_flat_idx[iflat]]
        if abs(to_shift) gt 1 then begin
           this_xdisp_d2 = shift(this_xdisp_d2, -to_shift)
           this_xdisp = shift(this_xdisp, -to_shift)
           to_shift = to_shift - fix(to_shift)
        endif
        
        shifted_flats_d2[*, iflat] = $
        interpol(this_xdisp_d2, xdisp_axis, $
                    xdisp_axis + to_shift)
        shifted_flats[*, iflat] = $
        interpol(this_xdisp, xdisp_axis, $
                    xdisp_axis + to_shift)
     endfor

     if keyword_set(TV) then $
       display, shifted_flats_d2, zoom=4, title='Shifted flats 2nd derivative'

     best_flat_d2 = fltarr(ny)
     for ixdisp=0, ny-1 do begin
        best_flat_d2[ixdisp] = median(shifted_flats_d2[ixdisp, *])
     endfor

     if keyword_set(plot) then begin
        window, 1, xs=640,ys=512
        plot, best_flat_d2, title='Best flat second derivative'
     endif

 if keyword_set(TV) then $
       display, shifted_flats_d2, zoom=4, title='Shifted flats 2nd derivative'

     best_flat = fltarr(ny)
     for ixdisp=0, ny-1 do begin
        best_flat[ixdisp] = median(shifted_flats[ixdisp, *])
     endfor

     if keyword_set(plot) then begin
        window, 1, xs=640,ys=512
        plot, best_flat, title='Best flat second derivative'
     endif


     message, 'Computing correlation between ueberflat and all files (including individual flats)', /INFORMATIONAL
     ;; Now find best peaks for all files.  Start over with our steps
     ;; parameter in case the user wants to specify that differently
     ;; between flats and data.  The most likely problem with slicer
     ;; position is a shift through the night.  Usually this is
     ;; gradual, sometimes it is sudden.  In either case, the flats
     ;; will typically catch both extremes and the ueber flat will be
     ;; in the middle somewhere.  The search for the best peak in the
     ;; correlation of the Io data should therefore not need to cover
     ;; as much ground.  Furthermore, since they don't have the nice
     ;; peaks o nthe edgets, covering too much ground can cause
     ;; spurious side peaks.  But just in case, put a command line
     ;; parameter in.
     nsteps = ny/10.
     if keyword_set(ndata_steps) then $
       nsteps = ndata_steps
     ;; When the CCD is binned, sometimes there are only 3 steps,
     ;; which is not enough for the algorithm to work well.  Try
     ;; making sure there are at least 5 steps.
     nsteps = nsteps > 5

     best_flat_correlates = fltarr(nsteps, nf)
     for ixdisp_shift=0, nsteps-1 do begin
        shift_d2 = shift(xdisps_d2, ixdisp_shift-nsteps/2., 0)
        for ifile=0, nf-1 do begin
           best_flat_correlates[ixdisp_shift, ifile] = $
             total(best_flat_d2 * shift_d2[*,ifile], /NAN)
        endfor
     endfor
     
     if keyword_set(TV) then $
       display, best_flat_correlates, zoom=[256/nsteps, 512/nf], $
                /reuse, title='Best flat correlations to files (X=steps, Y=files)'

     message, 'Finding peaks in correlations between ueberflat and each file', /INFORMATIONAL

     best_xdisp_peaks = fltarr(nf)
     best_xdisp_peaks_errors = fltarr(nf)
     for ifile=0,nf-1 do begin
        ;; Find peak in cross dispersion correlation.  Data seems
        ;; to have a good peak + climb back up towards secondary
        ;; peaks.  Start at the good peak and find the fist valley
        ;; on either side.
        xdisp_correlate = reform(best_flat_correlates[*, ifile])
        junk = max(xdisp_correlate, peak_idx)
        right = peak_idx + $
                first_peak_find(-xdisp_correlate[peak_idx:nsteps-1], $
                                'left', /poly, /quiet)
        left = first_peak_find(-xdisp_correlate[0:max([0,peak_idx-1])], $
                               'right', /poly, /quiet)
        best_xdisp_peaks[ifile] = $
          peak_find(xdisp_correlate[left:right], N_continuum=1, $
                    XTOL=1E5, FTOL=1E5, error=error) + left - nsteps/2.
        best_xdisp_peaks_errors[ifile] = error
     endfor

     ;;if keyword_set(plot) then begin
     ;;   window, 4, xs=640,ys=512
     ;;   ploterr, best_xdisp_peaks, best_xdisp_peaks_errors
     ;;endif
     

     message, 'Finding left and right sides of the ueberflat', /INFORMATIONAL
     ;; The first_peak_find default threshold and contrast is based on
     ;; the the maximum in the whole array.  In some cases,
     ;; e.g. reddir='/home/jpmorgen/data/ssg/reduced/2002/02jan26' 
     ;; the best flat second derivative is asymmetric enough to cause
     ;; first_peak_find to look at the entire flat execpt the left
     ;; peak as noise.  The signal on the flats tends to be very good

     best_flat_left = marks_edge_find(best_flat, /deviation, Secondderiv=best_flat_d2, /left, error=left_error)
     ;;best_flat_left  = first_peak_find(best_flat_d2, 'left', threshold=0.1, contrast=0.03)
     best_flat_right = marks_edge_find(best_flat, /deviation, Secondderiv=best_flat_d2, /right, error=right_error)
     ;;best_flat_right = first_peak_find(best_flat_d2, 'right', threshold=0.1, contrast=0.03)
     if best_flat_right - best_flat_left lt ny/10 then $
        message, 'ERROR: did not find sensible edges to the best flatfield [left, right]=[' + strtrim(left, 2) + ', ' + strtrim(right, 2)
     ;; This is in the wrong place, since best_xdisp_peaks can go
     ;; negative too and it might not be so bad to project best_flat
     ;; to where it is measured
     ;;if best_flat_left LT 0 THEN best_flat_left=0

     m_sli_bots[good_idx]  = best_flat_left + best_xdisp_peaks
     ;; Make sure slicer position doesn't go negative
     bad_idx = where(m_sli_bots[good_idx] lt 0, count)
     if count gt 0 then begin
        ;; unwrap
        bad_idx = good_idx[bad_idx]
        m_sli_bots[bad_idx] = 0
     endif
     e_sli_bots[good_idx]  = sqrt(best_xdisp_peaks_errors^2 + left_error^2)
     m_sli_tops[good_idx]  = best_flat_right + best_xdisp_peaks
     ;; Make sure slicer top doesn't go too high
     bad_idx = where(m_sli_tops[good_idx] ge ny, count)
     if count gt 0 then begin
        ;; unwrap
        bad_idx = good_idx[bad_idx]
        m_sli_tops[bad_idx] = ny-1
     endif
     e_sli_tops[good_idx]  = sqrt(best_xdisp_peaks_errors^2 + right_error^2)
     sli_cents [good_idx]  = (best_flat_left + best_flat_right)/2. + best_xdisp_peaks
     e_sli_cents[good_idx] = best_xdisp_peaks_errors

  endif ;; not reviewing


  if NOT keyword_set(noninteractive) then begin
     xtickunits='Hours'
     xtitle=string('UT time (Hours) ', utdate)
     title=string('Bottom pixel of slicer pattern ', indir)
     ytitle='Pixels from bottom of image'
     bot_marked_ndays = ssg_mark_bad (ndays, m_sli_bots, $
                                      measure_errors=e_sli_bots, $
                                      title=title, $
                                      xtickunits=xtickunits, $
                                      xtitle=xtitle, $
                                      ytitle=ytitle, $
                                      window=rwinnum, /MJD)
     
     title=string('Top pixel of slicer pattern ', indir)
     top_marked_ndays = ssg_mark_bad (ndays, m_sli_tops, $
                                      measure_errors=e_sli_tops, $
                                      title=title, $
                                      xtickunits=xtickunits, $
                                      xtitle=xtitle, $
                                      ytitle=ytitle, $
                                      window=rwinnum, /MJD)
     
     dbclose
  
     bad_idx = where(finite(bot_marked_ndays) eq 0 or $
                     finite(top_marked_ndays) eq 0, count)

     if count gt 0 then badarray[bad_idx] = badarray[bad_idx] OR 4096

     if NOT keyword_set(write) then begin
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'Write these values to the database?([Y]/N)'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'Y'
           answer = strupcase(answer)
        endrep until answer eq 'Y' or answer eq 'N'
        for ki = 0,1000 do flush_input = get_kbrd(0)
        if answer eq 'Y' then write=1
     endif

  endif ;; interactive


  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'bad, m_sli_bot, e_sli_bot, m_sli_top, e_sli_top, sli_cent, e_sli_cent', $
               badarray, m_sli_bots, e_sli_bots, m_sli_tops, e_sli_tops, sli_cents, e_sli_cents
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated slicer location values in ' + dbname + '.  Run ssg_fit_sliloc next'
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end
