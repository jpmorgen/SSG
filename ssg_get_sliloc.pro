;+
; $Id: ssg_get_sliloc.pro,v 1.6 2008/06/13 09:35:41 jpmorgen Exp $

; ssg_get_sliloc.  Find the top and bottom pixels (in Y) of the slicer
; pattern at the center in the image in the dispersion direction

;-

pro ssg_get_sliloc, indir, VERBOSE=verbose, TV=tv, showplots=showplots, zoom=zoom, pos=pos, write=write, noninteractive=noninteractive, review=review, window=winnum, rwindow=rwinnum, plot=plot, limits=in_limits

;  ON_ERROR, 2
  cd, indir

  silent = 1

  if keyword_set(verbose) then silent = 0
  if NOT keyword_set(winnum) then  winnum=6
  if NOT keyword_set(rwinnum) then rwinnum=7

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
  entries = dbfind(string("dir=", indir))

  dbext, entries, "fname, nday, date, typecode, bad, m_sli_bot, e_sli_bot, m_sli_top, e_sli_top, sli_cent, e_sli_cent", $
         files, ndays, dates, typecodes, badarray, m_sli_bots, e_sli_bots, m_sli_tops, e_sli_tops, sli_cents, e_sli_cents

dbclose

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

     good_idx = where(typecodes ge 2, nf)
     ;; Read in a file to get the size of the cross-dispersion array
     im = ssgread(files[0], hdr, eim, ehdr, /DATA, /TRIM)
     asize = size(im) & nx = asize(1) & ny = asize(2)
     xdisps_d2 = fltarr(ny, nf)
     for ifile=0,nf-1 do begin
        im = ssgread(files[good_idx[ifile]], hdr, eim, ehdr, /DATA, /TRIM)
        ssg_spec_extract, im, hdr, spec, xdisp, med_xdisp=y, /total        
        ;; For comps, use the total x-disp spectrum, though the second
        ;; derivative amplitude turns out to be significantly less
        if typecodes[ifile] eq 2 then begin
           y = xdisp
        endif
        dy = deriv(y)
        d2y = deriv(dy)
        if keyword_set(showplots) then begin
           plot, d2y
        endif
        xdisps_d2[*,ifile] = d2y
     endfor
     ;; histeq scaling works nicely.
     ;;atv, xdisps_d2
     display, xdisps_d2, zoom=4, /reuse

     ;; We have a 10 slicer
     correlates = fltarr(ny/10., nf, nf)
     xdisp_correlate = fltarr(ny/10.)
     xdisp_peaks = fltarr(nf, nf)
     for ifiles_shift=0,nf-1 do begin
        for ixdisp_shift=0, ny/10.-1 do begin
           ;; A little confusing with the -ifiles shift.  Use two
           ;; computer keyboards to illustrate why you want it this way.
           shift_d2 = shift(xdisps_d2, ixdisp_shift-ny/20., -ifiles_shift)
           for ifile=0, nf-1 do begin
              correlates[ixdisp_shift,ifiles_shift,ifile] = $
                total(xdisps_d2[*,ifile] * shift_d2[*,ifile])
           endfor
        endfor
     endfor

     for ifiles_shift=0,nf-1 do begin
        for ifile=0,nf-1 do begin
           ;; Find peak in cross dispersion correlation.  Data seems
           ;; to have a good peak + climb back up towards secondary
           ;; peaks.  Start at the good peak and find the fist valley
           ;; on either side.
           xdisp_correlate = reform(correlates[*, ifiles_shift, ifile])
           junk = max(xdisp_correlate, peak_idx)
           right = peak_idx + $
                   first_peak_find(-xdisp_correlate[peak_idx:ny/10.-1], $
                                   'left', /poly, /quiet)
           left = first_peak_find(-xdisp_correlate[0:peak_idx-1], $
                                  'right', /poly, /quiet)
           xdisp_peaks[ifiles_shift, ifile] = $
             peak_find(xdisp_correlate[left:right], /poly) + left - ny/20.
        endfor
        message, /INFO, 'Finished with shift:' + strtrim(ifiles_shift, 2)
     endfor

     ;;atv, xdisp_peaks
     ;;stop
     display, xdisp_peaks, zoom=4, /reuse

     peak_meds = fltarr(nf)
     peak_means = fltarr(nf)
     ;;peak_flat_meds = fltarr(nf)
     ;;peak_flat_means = fltarr(nf)
     for ifile=0, nf-1 do begin
        peak_meds[ifile] = median(xdisp_peaks[*, ifile])
        peak_means[ifile] = mean(xdisp_peaks[*, ifile])
        ;; Get median and mean offsets of each file from the flats
     endfor
     ;;window,0
     ;;plot, peak_meds, yrange=[-ny/20., ny/20]
     ;;oplot, peak_means, linestyle=dashed

     ;; Make an "ueber flat"
     flat_idx = where(typecodes[good_idx] eq 3, nflats)
     xdisp_axis = indgen(ny)

     shifted_flats = fltarr(ny, nflats)
     for iflat=0, nflats-1 do begin
        shifted_flats[*, iflat] = interpol(xdisps_d2[*, flat_idx[iflat]], xdisp_axis, $
                                           xdisp_axis + peak_meds[flat_idx[iflat]])
     endfor

     display, shifted_flats, zoom=4

     best_flat_d2 = fltarr(ny)
     for ixdisp=0, ny-1 do begin
        best_flat_d2[ixdisp] = median(shifted_flats[ixdisp, *])
     endfor

     window,0
     plot, best_flat_d2

     best_flat_correlates = fltarr(ny/10., nf)
     for ixdisp_shift=0, ny/10.-1 do begin
        shift_d2 = shift(xdisps_d2, ixdisp_shift-ny/20., 0)
        for ifile=0, nf-1 do begin
           best_flat_correlates[ixdisp_shift, ifile] = $
             total(best_flat_d2 * shift_d2[*,ifile])
        endfor
     endfor
     
     display, best_flat_correlates, zoom=4

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
                first_peak_find(-xdisp_correlate[peak_idx:ny/10.-1], $
                                'left', /poly, /quiet)
        left = first_peak_find(-xdisp_correlate[0:peak_idx-1], $
                               'right', /poly, /quiet)
        best_xdisp_peaks[ifile] = $
          peak_find(xdisp_correlate[left:right], /poly, $
                    error=error) + left - ny/20.
        best_xdisp_peaks_errors[ifile] = error
     endfor

     wset, 0
     ploterr, best_xdisp_peaks, best_xdisp_peaks_errors
     
     best_flat_left  = first_peak_find(best_flat_d2, 'left')
     best_flat_right = first_peak_find(best_flat_d2, 'right')

     m_sli_bots  = best_flat_left + best_xdisp_peaks
     e_sli_bots  = best_xdisp_peaks_errors
     m_sli_tops  = best_flat_right + best_xdisp_peaks
     e_sli_tops  = best_xdisp_peaks_errors
     sli_cents   = (best_flat_left + best_flat_right)/2. + best_xdisp_peaks
     e_sli_cents = best_xdisp_peaks_errors

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
