; $Id: ssg_mark_bad.pro,v 1.1 2002/12/16 13:37:10 jpmorgen Exp $

; ssg_markbad.pro.  displays a graph like jpm_polyfit + allows the
; user to display images + spectra (left button) mark things as bad
; (middle button), and exit (right button)


function ssg_mark_bad, x, y, title=title, noninteractive=noninteractive, window=winnum, spec_winnum=spec_winnum, xtitle=xtitle, ytitle=ytitle, xtickunits=xtickunits, measure_errors=measure_errors, legend=legend_text, reuse=reuse

  if NOT keyword_set(winnum) then winnum=7
  if NOT keyword_set(spec_winnum) then spec_winnum=6
  ;; check to see if y is a set of axes to plot

  ;; I am not sure if this fiddling is necessary, but I do want to
  ;; have simple code below in the loop
  py = y
  nplots = size(y, /N_DIMENSIONS) 
  if nplots eq 0 then py = make_array(1,1, value=y)
  if nplots eq 1 then begin
     py = make_array(N_elements(y),1)
     py[*,0] = y
  endif
  nplots = size(y, /N_DIMENSIONS)

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

  window,spec_winnum, title='Spectral preview'
  window,winnum, title=title
  nmarked = 0
  repeat begin

     redisp = 1
     wset,winnum
     good_idx = where(finite(x) eq 1 and finite(y) eq 1, count)
     if count eq 0 then return, x
     plot, x[good_idx], py[good_idx,0], $
           title=title, $
           xtickunits=xtickunits, $
           xrange=[min(x[good_idx], /NAN), $
                   max(x[good_idx], /NAN)], $
           yrange=[min(py[good_idx,0], /NAN), $
                   max(py[good_idx,0], /NAN)], $
           xstyle=2, ystyle=2, psym=plus, $
           xtitle=xtitle, $
           ytitle=ytitle
;           xstyle=2, ystyle=2, psym=plus, $
     if keyword_set(measure_errors) then $
       oploterr, x[good_idx], py[good_idx,0], measure_errors[good_idx,0]

     psymlist = mypsym[0]
     for pi = 1, nplots-1 do begin
        psymlist = [psymlist, mypsym[pi mod N_elements(mypsym)]]
        oplot, x[good_idx], py[good_idx,pi], psym=psymlist[pi]
        if keyword_set(measure_errors) then $
          oploterr, x[good_idx], py[good_idx,pi], measure_errors[good_idx,pi]
     endfor
     if keyword_set(legend_text) then $
       legend, legend_text, psym=psymlist

     ;; User selects a bad point
     message, /CONTINUE, 'Use left button to select a single point.  Lots of information will be displayed.  If you left drag over may points, they will be selected without displaying information.  Press the middle button to mark the currently selected point(s) as bad.  It/they will be removed from the graph.  The right button to exits.'
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
              if x1 lt x[xidx] and x[xidx] lt x2 and $
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
           dxs = x - x1
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
        
     endif ;; leftmost mouse button

     if !MOUSE.button eq 2 then begin

        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'Mark ' + string(nmarked) + ' point(s) as bad (Y/[N])'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'N'
           answer = strupcase(answer)
        endrep until answer eq 'Y' or answer eq 'N'
        if answer eq 'Y' then begin
           x[marked_idx] = !values.f_nan
           nmarked = get_kbrd(1)
        endif
        for ki = 0,1000 do flush_input = get_kbrd(0)
     endif

     if !MOUSE.button eq 4 then begin
        message, /CONTINUE, 'DONE'
        redisp = 0
     endif
  endrep until redisp eq 0


return, x

end
