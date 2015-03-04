; $Id: ssg_mark_bad.pro,v 1.5 2015/03/04 15:45:14 jpmorgen Exp $

; ssg_mark_bad.pro.  displays a graph like jpm_polyfit + allows the
; user to display images + spectra and mark them as bad


function ssg_mark_bad, x, y, title=title, noninteractive=noninteractive, window=winnum, spec_winnum=spec_winnum, xtitle=xtitle, ytitle=ytitle, xtickunits=xtickunits, measure_errors=measure_errors, legend=legend_text, reuse=reuse, MJD=MJD

  init = {ssg_sysvar}
  if NOT keyword_set(winnum) then winnum=7
  if NOT keyword_set(spec_winnum) then spec_winnum=6
  ;; check to see if y is a set of axes to plot

  x_save = x
  bad_stack = intarr(N_elements(x)+1)
  bad_stack[*] = -1

  ;; I am not sure if this fiddling is necessary, but I do want to
  ;; have simple code below in the loop
  py = y
  nplots = size(y, /N_DIMENSIONS) 
  if nplots eq 0 then py = make_array(1,1, value=y)
  if nplots eq 1 then begin
     py = make_array(N_elements(y),1)
     py[*,0] = y
  endif
  if nplots eq 2 then begin
     asize = size(y)
     nplots = asize[2]
  endif

  if keyword_set(legend_text) and N_elements(legend_text) ne nplots then $
    message, 'ERROR: legend needs to have the same number Yaxes you have'


  plus = 1
  asterisk = 2
  dot = 3
  diamond = 4
  triangle = 5
  square = 6
  psym_x = 7

  mypsym = [plus, asterisk, diamond, triangle, square, psym_x]

  solid=0
  dotted=1
  dashed=2
  dash_dot=3
  dash_3dot = 4
  long_dash=5

  nmarked = 0
  window,spec_winnum, title='Spectral preview'
  window,winnum, title=title

  ;; Ndays are referenced to modified Julian day.  IDL plotting
  ;; routines are referenced to JD, so there needs to be an offset in
  ;; the displayed values, but not in the calulated values.
  plotx = x
  if keyword_set(MJD) then $
    plotx = x-0.5

  repeat begin
     redisp = 1
     wset,winnum
     good_idx = where(finite(x) eq 1 and $
                      finite(y) eq 1, count)
     if keyword_set(measure_errors) then $
       good_idx = where(finite(x) eq 1 and $
                        finite(y) eq 1 and $
                        measure_errors gt 0, count)
     if count eq 0 then return, x
     plot, plotx[good_idx], py[good_idx,0], $
           title=title, $
           xtickunits=xtickunits, $
           xrange=[min(plotx[good_idx], /NAN), $
                   max(plotx[good_idx], /NAN)], $
           yrange=[min(py[good_idx,*], /NAN), $
                   max(py[good_idx,*], /NAN)], $
           xstyle=2, ystyle=2, psym=plus, $
           xtitle=xtitle, $
           ytitle=ytitle
;           xstyle=2, ystyle=2, psym=plus, $
     if keyword_set(measure_errors) then $
       oploterr, plotx[good_idx], py[good_idx,0], measure_errors[good_idx,0]

     psymlist = mypsym[0]
     for pi = 1, nplots-1 do begin
        psymlist = [psymlist, mypsym[pi mod N_elements(mypsym)]]
        oplot, plotx[good_idx], py[good_idx,pi], psym=psymlist[pi]
        if keyword_set(measure_errors) then $
          oploterr, x[good_idx], py[good_idx,pi], measure_errors[good_idx,pi]
     endfor
     if keyword_set(legend_text) then $
       al_legend, legend_text, psym=psymlist

     ;; User selects a bad point
     message, /CONTINUE, 'Use left button to select a single point; lots of information will be displayed.  If you left drag over may points, they will be selected without displaying information.  Middle button brings up menu that lets you exit, delete selected points, or resurrect all deleted points.   Right button resurrects points one-by-one that were marked as bad in this session.'
     cursor, x1, y1, /DOWN, /DATA
     cursor, x2, y2, /UP, /DATA
     ;; Get the corners straight
     if x1 gt x2 then begin
        temp = x1 & x1 = x2 & x2 = temp
     endif
     if y1 gt y2 then begin
        temp = y1 & y1 = y2 & y2 = temp
     endif
     if !MOUSE.button eq 1 then begin
        ;; The drag group gets a little complicated with IDL's
        ;; sophisicated array handling, so lets do it the old
        ;; fashioned way
        nmarked = 0
        for xidx = 0, N_elements(x) - 1 do begin
           for pidx = 0,nplots-1 do begin
              if x1 lt plotx[xidx] and plotx[xidx] lt x2 and $
                y1 lt y[xidx,pidx] and y[xidx,pidx] lt y2 then begin
                 ;; Check initialization of marked_idx
                 if nmarked eq 0 then begin
                    marked_idx = [xidx] 
                 endif else begin
                    ;; Add this entry in if it is not already there
                    junk = where(marked_idx eq xidx, count)
                    if count eq 0 then $
                      marked_idx = [marked_idx, xidx]
                 endelse
                 nmarked = N_elements(marked_idx)
              endif
           endfor
        endfor
        ;; We didn't find any points in our region
        if nmarked eq 0 then begin
           dxs = plotx - x1
           dys = fltarr(N_elements(x), nplots)
           for pi = 0, nplots-1 do begin
              dys[*,pi] = py[*,pi] - y1
           endfor
           dists = fltarr(N_elements(x), nplots)
           for pi = 0, nplots-1 do begin
              dists[*,pi] = dxs^2 + dys[*,pi]^2
           endfor
           junk = min(dists, min_idx, /NAN)
           ;; Unwrap index
           marked_idx = min_idx mod N_elements(x)
           nmarked = N_elements(marked_idx)
        endif
        message, /info, 'X value(s) at selection'
        print, x[marked_idx]
        message, /info, 'Y value(s) at selection'
        print, py[marked_idx,*]

        entries = where_nday_eq(x(marked_idx), count=count)
        if count eq 0 then begin
           message, /CONTINUE, 'ERROR: no match found for you selection.  Is your database still open?  Were you plotting against nday?'
        endif else begin
           dbext, entries, "fname", files
           if N_elements(files) ne nmarked then $
             message, 'ERROR: there was some problem with the database?'
           if nmarked eq 1 then begin
              im = ssgread(files[0], hdr)
              display, im, hdr, title=files, reuse=reuse
              wset, spec_winnum
              ssg_spec_extract, im, hdr, /AVERAGE, /show, title=files[0]
              print, hdr
           endif 
           if nmarked gt 1 then $
             message, /CONTINUE, 'Selecting ' + string(nmarked) + ' entries.'
        endelse
     endif ;; mouse 1

     if !MOUSE.button eq 2 and nmarked eq 0 then begin
        for ki = 0,1000 do flush_input = get_kbrd(0)
        message, /CONTINUE, 'No points marked.  Does that mean you want to:'
        print, 'Quit/eXit (permanent write can be avoided later)'
        print, 'Continue'
        print, 'resurect All points'
        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, '[Q/X], C, A?'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'Q'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           answer = strupcase(answer)
        endrep until $
          answer eq 'Q' or $
          answer eq 'X' or $
          answer eq 'C' or $
          answer eq 'A'

        if answer eq 'A' then begin
           x = x_save
           if keyword_set(measure_errors) then $
             measure_errors = abs(measure_errors)
        endif ;; A

        if answer eq 'Q' or answer eq 'X' then begin
           message, /CONTINUE, 'DONE'
           return, x
        endif ;; Q/X

     endif  ;; Mouse 2 and nothing marked

     if !MOUSE.button eq 2 and nmarked gt 0 then begin
        if keyword_set(measure_errors) then begin
           message, /CONTINUE, 'Preparing to mark ' + string(nmarked) + ' point(s) as bad'
           print, '(M) bad Measurement in an otherwise good file (point _will_ be replaced by polynominal fit by jpm_polyfit _but_ you will see the point there and it will weight the fit unless you explicitly mark it in jpm_polyfit.)'
           print, '(F) bad File (will be excluded from all subsequent analysis)'
           print, '(Q) quit menu with no changes'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, '[M], F, Q?'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'M'
              for ki = 0,1000 do flush_input = get_kbrd(0)
              answer = strupcase(answer)
           endrep until $
             answer eq 'M' or $
             answer eq 'F' or $
             answer eq 'Q'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           if answer eq 'M' then begin
              if nplots gt 1 then begin
                 message, /CONTINUE, 'ERROR: sorry, can''t select this option in multiple plot mode yet.  Modify upstream code to give me just one plot at a time'
              endif else begin
                 measure_errors[marked_idx] = -abs(measure_errors[marked_idx])
              endelse
           endif

           if answer eq 'F' then begin
              x[marked_idx] = !values.f_nan
           endif

        endif else begin
           ;; NO ERROR BARS
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'Mark ' + string(nmarked) + ' point(s) as permanently bad (Y/[N])'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'N'
              answer = strupcase(answer)
           endrep until answer eq 'Y' or answer eq 'N'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           if answer eq 'Y' then begin
              x[marked_idx] = !values.f_nan
           endif
        endelse ;; without error bars

        ;; Remember which things we marked as bad so we can resurrect them
        for i=0,nmarked-1 do begin
           push, marked_idx[i], bad_stack, null=-1
        endfor
        nmarked = 0

     endif ;; Mouse 2

     if !MOUSE.button eq 4 then begin
        idx = pop(bad_stack, null=-1)
        if idx eq -1 then begin
           message, /CONTINUE, 'ALL POINTS RESURRECTED'
        endif else begin
           x[idx] = x_save[idx]
           if keyword_set(measure_errors) then $
             measure_errors[idx] = abs(measure_errors[idx])
        endelse
     endif ;; Mouse 4


  endrep until redisp eq 0


return, x

end
