;+
; $Id: ssg_select.pro,v 1.4 2014/03/24 15:43:51 jpmorgen Exp $

; ssg_select.  Displays relevant information from the databases to try
; to help a user select one or more spectra to fit, grab parameters
; from, etc.

;-


function ssg_select, nday_start_or_range, count=count, title=title, _EXTRA=extra

;  ON_ERROR, 2

  init = {tok_sysvar}

  if NOT keyword_set(title) then title='SSG select'
  if NOT keyword_set(nday_start_or_range) then $
    nday_start_or_range = [0,36500] ; Somewhat generous :-)

  nday_range = nday_start_or_range
  if N_elements(nday_range) eq 1 then $
    nday_range = [nday_start_or_range, 36500]

  ;; Behave like the db stuff
  if nday_range[0] eq -1 then $
    nday_range = [0,36500]

  nday_range_orig = nday_range

  silent = 1
  if keyword_set(verbose) then silent = 0

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

  rdbname = 'ssg_reduce'
  fdbname = 'oi_6300_fit'

  window, 7

  ;; Extract list of valid ndays from the fitting database 
  dbclose                       ; Just in case
  dbopen, fdbname, 0
  fentries = where_nday_eq(mean(nday_range), $
                           tolerance=abs(nday_range[1]-nday_range[0])/2., $
                           count=fcount, silent=silent)
  if fcount eq 0 then begin
     message, /CONTINUE, 'WARNING: no entries in fitting database between ' + string(nday_range[0]) + ' and ' + string(nday_range[1]) + ' returning nday = -1.  Did you run ssg_extract first?'
     count = 0
     return, -1
  endif

  dbext, fentries, 'nday, new_spec, fit_vers, bad, nfree, chisq, redchisq', ndays, new_specs, fit_vers, fbaddarray, nfrees, chisqs, redchisqs

  ;; Extract these entries from the reduction database

  dbopen, rdbname, 0
  rentries = where_nday_eq(ndays, count=rcount, silent=silent)
  if rcount ne fcount then begin
     ;;message,  'ERROR: entires in fitting database do not have corresponding entries in reduction database.  This is a bad thing.  Rerunning ssg_extract might fix this'
     message, /CONTINUE, 'WARNING: entires in fitting database do not have corresponding entries in reduction database.  Average/median values might not line up properly with fitting information.  Rerunning ssg_extract on the offending day might fix this'

  endif

  dbext, rentries, "fname, date, typecode, bad, nbad, ncr", files, dates, typecodes, badarray, nbads, ncrs

  dbext, rentries, "med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross", med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps
  dbclose

  ;; Prepare nday range.  IDL handles UT for plotting better, so
  ;; keep a parallel variable around
  nday_range = [min(ndays), max(ndays)]
  uts = ndays + (julday(1,1,1990,0))
  ut_range = nday_range + (julday(1,1,1990,0))

  ;; Select spectr[uma] to fit
  done = 0
  repeat begin
     wset,7
     yrange = [0, max([med_specs,av_specs,fit_vers*10])]
     good_idx = where(ut_range[0] le uts and uts le ut_range[1], count)
     if count gt 0 then begin
        yrange = [0, max([med_specs[good_idx],av_specs[good_idx], $
                          fit_vers[good_idx]*10])]
     endif
     plot, uts, med_specs, $
           title=title, $
           xrange=ut_range, $
           yrange=yrange, $
           xtickunits = ['hours', 'Days', 'Months', 'Years'], $
           xtitle='UT date (Year, Month, Day, Hour)', $
           ytitle='Spectral value (electrons/s)', $
           ymargin=[15,2], $
           psym=plus, $ 
           xstyle=!tok.exact+!tok.extend, $
           _EXTRA=extra
     oplot, uts, av_specs, psym=asterisk
     oplot, uts, new_specs*10, psym=diamond
     oplot, uts, fit_vers*10, psym=triangle
     oplot, uts, redchisqs, psym=square

     legend, ['Median spectral value', 'Average spectral value', 'NEW SPECTRUM (0=no, 10=yes)', 'Fit version(*10)', 'Reduced Chi Square of fit'], psym=[plus, asterisk, diamond, triangle, square]

     ;; Steal code from ssg_mark_bad.  -->  Some day I might unify
     ;; these, but I am in a hurry right now + they do have somewhat
     ;; different functions
     x = ndays
;     y = [[med_specs], [av_specs], [new_specs*10], [fit_vers*10]]
;     py=y
     nplots=4

     ;; User selects points
     message, /CONTINUE, 'Use left and right buttons to zoom in on (bracket) area of interest. Middle button will bring up a menu.'

     cursor, ut, y, /DOWN, /DATA

     ;; Left mouse
     if !MOUSE.button eq 1 then begin
        ut_range[0] = ut
     endif
     ;; Right mouse
     if !MOUSE.button eq 4 then begin
        ut_range[1] = ut
     endif
     nday = ut - (julday(1,1,1990,0))
     nday_range = ut_range - (julday(1,1,1990,0))
     ;; Middle mouse
     if !MOUSE.button eq 2 then begin
        message, /CONTINUE, 'Menu:'
        print, 'exit with selected Range'
        print, 'exit with selected Point'
        print, 'Quit/exit with no selection'
        print, 'unZoom'
        print, 'do Nothing'
        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'R, P, Q, Z, N'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'F'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           answer = strupcase(answer)
        endrep until $
          answer eq 'R' or $
          answer eq 'P' or $
          answer eq 'Q' or $
          answer eq 'Z' or $
          answer eq 'N'

        ;; Return all ndays within plot range
        if answer eq 'R' then begin
           r_idx = where(nday_range[0] le ndays and ndays le nday_range[1], $
                         count)
           return, ndays[r_idx]
        endif

        ;; Return nday closest to where middle mouse was clicked
        if answer eq 'P' then begin
           dists = abs(ndays - nday)
           junk = min(dists, min_idx, /NAN)
           count = N_elements(min_idx)
           return, ndays[min_idx]
        endif

        ;; Return -1, setting count=0
        if answer eq 'Q' then begin
           count = 0
           return, -1
        endif

        ;; unZoom
        if answer eq 'Z' then begin
           nday_range = [min(ndays), max(ndays)]
           ut_range = nday_range + (julday(1,1,1990,0))
        endif

     endif ;; Middle mouse


;;     ;;cursor, x2, y2, /UP, /DATA
;;     ;; Get the corners straight
;;     if x1 gt x2 then begin
;;        temp = x1 & x1 = x2 & x2 = temp
;;     endif
;;     if y1 gt y2 then begin
;;        temp = y1 & y1 = y2 & y2 = temp
;;     endif
;;     ;; Convert from JD, which I am using for convenient plotting to nday
;;     x1 = x1 - (julday(1,1,1990,0))
;;     x2 = x2 - (julday(1,1,1990,0))
;;     if !MOUSE.button eq 1 then begin
;;        ;; The drag group gets a little complicated with IDL's
;;        ;; sophisicated array handling, so lets do it the old
;;        ;; fashioned way
;;        nmarked = 0
;;        for xidx = 0, N_elements(x) - 1 do begin
;;           for pidx = 0,nplots-1 do begin
;;              if x1 lt x[xidx] and x[xidx] lt x2 then begin ;; and $
;;;;                y1 lt y[xidx,pidx] and y[xidx,pidx] lt y2 then begin
;;                 ;; Check initialization of marked_idx
;;                 if nmarked eq 0 then begin
;;                    marked_idx = [xidx] 
;;                 endif else begin
;;                    ;; Add this entry in if it is not already there
;;                    junk = where(marked_idx eq xidx, count)
;;                    if count eq 0 then $
;;                      marked_idx = [marked_idx, xidx]
;;                 endelse
;;                 nmarked = N_elements(marked_idx)
;;              endif
;;           endfor
;;        endfor
;;        ;; We didn't find any points in our region
;;        if nmarked eq 0 then begin
;;           dxs = x - x1
;;; This was causing unexpected results.  Just go with which x you are
;;; closest to.
;;;            dys = fltarr(N_elements(x), nplots)
;;;            for pi = 0, nplots-1 do begin
;;;               dys[*,pi] = py[*,pi] - y1
;;;            endfor
;;;            dists = fltarr(N_elements(x), nplots)
;;;            for pi = 0, nplots-1 do begin
;;;               dists[*,pi] = dxs^2 + dys[*,pi]^2
;;;            endfor
;;;            junk = min(dists, min_idx, /NAN)
;;;            ;; Unwrap index
;;;            marked_idx = min_idx mod N_elements(x)
;;;            nmarked = N_elements(marked_idx)
;;           dists = abs(dxs)
;;           junk = min(dists, min_idx, /NAN)
;;           marked_idx = min_idx 
;;           nmarked = N_elements(marked_idx)
;;        endif
;;        message, /info, 'X value(s) at selection'
;;        print, x[marked_idx]
;;;        message, /info, 'Y value(s) at selection'
;;;        print, py[marked_idx,*]
;;        
;;        ;; Open the fit database again, since that is where we got the
;;        ;; x list from
;;        dbopen, fdbname, 0
;;        entries = where_nday_eq(x[marked_idx], count=count, silent=silent)
;;        if count eq 0 then begin
;;           message, /CONTINUE, 'ERROR: no match found for you selection.  Is your database still open?  Were you plotting against nday?'
;;        endif else begin
;;           dbext, entries, "nday", marked_ndays
;;           if N_elements(marked_ndays) ne nmarked then begin
;;              message, 'ERROR: there was some problem with the database?'
;;           endif
;;           if nmarked eq 1 then begin
;;              dbclose
;;              return, marked_ndays
;;           endif 
;;           if nmarked gt 1 then begin
;;              message, /CONTINUE, 'Zooming on ' + string(nmarked) + ' points.'
;;              nday_range = [min(marked_ndays), max(marked_ndays)]
;;           endif
;;        endelse
;;        
;;     endif ;; leftmost mouse button
;;
;;     if !MOUSE.button eq 2 then begin
;;        nday_range = [0,36500]
;;     endif
;;
;;     if !MOUSE.button eq 4 then begin
;;        message, /CONTINUE, 'DONE'
;;        if NOT keyword_set(multi) then begin
;;           ndays = -1
;;        endif
;;        done = 1
;;     endif

  endrep until done

end

