;+
; $Id: ssg_get_sliloc.pro,v 1.4 2003/06/11 18:11:42 jpmorgen Exp $

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
  ;; Get all the files in the directory so we can mark camrot as not
  ;; measured on the ones where we can't measure it.
  entries = dbfind(string("dir=", indir))

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

     ;; Make a loop to do the flats first, from which we get a good
     ;; idea of where to look for the edges in the objects and comps,
     ;; where edges don't show as well.
     repeat begin
     for i=0,nf-1 do begin
        message, 'Looking at ' + files[i], /CONTINUE
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           if badarray[i] ge 8192 then message, 'BAD FILE, use display, ssg_spec_extract, and look at the header if you are unsure why'
           if typecodes[i] lt 2 then message, 'Skipping bias/dark images'

           ;; Set threshold and contrast for flats + override if we
           ;; are looking at something else
           threshold = 1.       ; Use the max
           contrast = 0.6       ; Flats always have good contrast
           if (typecodes[i] le 2 or typecodes[i] ge 5) then begin
              ;; Check to see if we are getting our preliminary edge position
              if NOT keyword_set(good_lim) then $
                message, 'Skipping object/comp until a good preliminary edge position is found'
              ;; Threshold still wants to be 1 for the max, but the
              ;; contrast is generally much worse on these, so only go
              ;; a little ways down the peak
              threshold = 1.
              contrast = 0.2
           endif
           CATCH, /cancel

           im = ssgread(files[i], hdr, eim, ehdr, /DATA, /TRIM)
           biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
           if count eq 0 then message, 'WARNING: works better if you call ssg_biassub first', /CONTINUE

           asize = size(im) & nx = asize(1) & ny = asize(2)

           ;; For continuum dominated spectra (Io), use the median
           ;; cross-dispersion spectrum
           ssg_spec_extract, im, hdr, spec, xdisp, med_xdisp=y, /total
           ssg_spec_extract, eim^2, hdr, med_xdisp=ey2, /total

           ;; For comps, use the total x-disp spectrum
           if typecodes[i] eq 2 then y = xdisp
           
           ;; We want to find the first and last edges in the
           ;; cross-dispersion direction.  I have a whole system for
           ;; doing this called edge find.  It works best if it is
           ;; pased the error bars of the original points.  Since we
           ;; have converted to electrons,  these should be the square
           ;; root of the counts in those channels
           npts = N_elements(y)
           if keyword_set(in_limits) then begin
              limits = in_limits
           endif else begin
              if keyword_set(good_lim) then begin
                 limits = good_lim
              endif else begin
                 limits = [0,npts/2.,npts/2,npts-1]
              endelse
           endelse
           if limits[0] lt 0 then limits[0] = 0
           if limits[1] ge npts then begin
              message, /CONTINUE, 'WARNING: strange value on sli_bot upper limit'
              limits[1] = npts-1
           endif
           if limits[1] ge npts then begin
              message, /CONTINUE, 'WARNING: strange value on sli_top lower limit'
              limits[1] = 0
           endif
           if limits[3] ge npts then limits[3] = npts-1

           if keyword_set(plot) then $
             title = "Slicer Bottom, derivative"
           m_sli_bots[i]  = ssg_edge_find(y, 'left', threshold=threshold, $
                                          contrast=contrast, $
                                          limits=[limits[0],limits[1]], $
                                          yerr=sqrt(ey2), error=temp, $
                                          plot=title)

           e_sli_bots[i] = temp

           if keyword_set(plot) then begin
              wait, 0.3
              title = "Slicer Top, derivative"
           endif
           m_sli_tops[i] = ssg_edge_find(y, 'right', threshold=threshold, $
                                         contrast=contrast, $
                                         limits=[limits[2],limits[3]], $
                                         yerr=sqrt(ey2), error=temp, $
                                         plot=title)

           e_sli_tops[i] = temp
           ngood = ngood + 1
           if keyword_set(showplots) then begin
              wait, 0.3
              plot, y, title='cross-dispersion spectrum'
              plots, [m_sli_bots[i], m_sli_bots[i]], [-1E32, 1E32]
              plots, [m_sli_tops[i], m_sli_tops[i]], [-1E32, 1E32]
           endif
        endelse ;; CATCH if err
     endfor ;; all files in directory
     ;; Use the flatfield edges to define a small region to search
     ;; over for the rest of the images.  Take a little less than a
     ;; slice in each direction
     if NOT keyword_set(good_lim) then begin
        good_lim = [median(m_sli_bots),median(m_sli_tops)]
        delta = good_lim[1]-good_lim[0]
        good_lim = [good_lim[0]-delta/15., good_lim[0]+delta/15., $
                    good_lim[1]-delta/15., good_lim[1]+delta/15.]
        
     endif else begin
        good_lim = -1
     endelse
     endrep until N_elements(good_lim) eq 1
     CATCH, /CANCEL
     if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

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
