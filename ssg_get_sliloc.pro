;+
; $Id: ssg_get_sliloc.pro,v 1.3 2003/05/01 17:55:33 jpmorgen Exp $

; ssg_get_sliloc.  Find the top and bottom pixels (in Y) of the slicer
; pattern at the center in the image in the dispersion direction

;-

pro ssg_get_sliloc, indir, VERBOSE=verbose, TV=tv, showplots=showplots, zoom=zoom, pos=pos, nsteps=nsteps, contrast=contrast_in, write=write, noninteractive=noninteractive, review=review, window=winnum, rwindow=rwinnum, bot_lim=bot_lim_in, top_lim=top_lim_in, plot=plot


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

     for i=0,nf-1 do begin
        message, 'Looking at ' + files[i], /CONTINUE
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           if badarray[i] ge 8192 then message, 'BAD FILE, use display, ssg_spec_extract, and look at the header if you are unsure why'
           if typecodes[i] lt 2 then message, 'It doesn''t make sense to get a slicer center from bias or dark images'
CATCH, /cancel
           im = ssgread(files[i], hdr, eim, ehdr, /DATA, /TRIM)
           biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
           if count eq 0 then message, 'WARNING: works better if you call ssg_biassub first', /CONTINUE

           ;; Contrast is the contrast on the first derivative of the
           ;; cross dispersion spectrum, so it can be a bit higher
           ;; than you'd think just looking at the function itself.  A
           ;; contrast of 0.1 seems to be too low for rough comps,
           ;; since it catches the very edge of the light outside of
           ;; the slicer.
           if NOT keyword_set(contrast_in) then begin
              ;; Unbinned measurements don't have the greatest S/N
              contrast_in=0.05
              ;; Binned measurments have better S/N
              ccdsum = strtrim(sxpar(hdr,'CCDSUM',COUNT=count),2)
              if count gt 0 then begin
                 if ccdsum eq '1 4' then begin
                    contrast_in = 0.01
                 endif
              endif
           endif
           contrast = contrast_in
           if typecodes[i] eq 2 then contrast = max([10*contrast, .9])


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
           if keyword_set(bot_lim_in) then $
             bot_lim = bot_lim_in $
           else $
             bot_lim = npts/4
           if keyword_set(top_lim_in) then $
             top_lim = top_lim_in $
           else $
             top_lim = bot_lim*1.5
           top_lim = npts - top_lim
           bot_y =  y[0:bot_lim]
           top_y = y[top_lim:npts-1]
           bot_ey =  sqrt(ey2[0:bot_lim])
           top_ey = sqrt(ey2[top_lim:npts-1])
           
           if keyword_set(plot) then $
             title = "Slicer Bottom, derivative"
           m_sli_bots[i]  = edge_find(bot_y, 'left' , contrast=contrast, $
                                      yerr=top_ey, error=temp, $
                                      plot=title) $
                            + 0 
           e_sli_bots[i] = temp

           if keyword_set(plot) then begin
              wait, 1
              title = "Slicer Top, derivative"
           endif
           m_sli_tops[i] = edge_find(top_y, 'right', contrast=contrast, $
                                     yerr=bot_ey, error=temp, $
                                     plot=title) $
                           + top_lim
           e_sli_tops[i] = temp
           ngood = ngood + 1
           if keyword_set(showplots) then begin
              wait, 1
              plot, y, title='cross-dispersion spectrum'
              plots, [m_sli_bots[i], m_sli_bots[i]], [-1E32, 1E32]
              plots, [m_sli_tops[i], m_sli_tops[i]], [-1E32, 1E32]
              wait, 1
           endif
              
        endelse ;; CATCH if err
     endfor ;; all files in directory
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
