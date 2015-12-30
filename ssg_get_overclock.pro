;+
; $Id: ssg_get_overclock.pro,v 1.7 2003/06/11 19:57:20 jpmorgen Exp jpmorgen $

; ssg_get_overclock.  collects information on the CCD overclock region
; to put into the reduction database.

;-

pro ssg_get_overclock, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, pos=pos, write=write, noninteractive=noninteractive, review=review

;  ON_ERROR, 2
  cd, indir

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


  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  
  entries = dbfind(string("dir=", indir), /fullstring)
  dbext, entries, "fname, nday, date, bad, n_ovrclk, med_ovrclk, av_ovrclk, med_bias, av_bias, stdev_bias", files, ndays, dates, badarray, n_ovrclk, med_ovrclk, av_ovrclk, med_bias, av_bias, stdev_bias

  files=strtrim(files)
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  

  if NOT keyword_set(review) then begin ; We really want to do all the file reading


     if keyword_set(showplots) then begin
        window, 6
        window, 7
     endif
     err=0

     med_bias   [*]   = !values.f_nan
     av_bias    [*]   = !values.f_nan
     stdev_bias [*]   = !values.f_nan
     med_ovrclk [*,*] = !values.f_nan
     av_ovrclk  [*,*] = !values.f_nan

     for i=0,nf-1 do begin
        message, 'Looking at ' + files[i], /CONTINUE
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           if badarray[i] ge 16384 then $
             message, 'ERROR: file has been marked bad for an unspecified reason.'
           if n_ovrclk[i] ne 0 then $
             message, /CONTINUE, 'WARNING: bias info already in database'
           ;; Make sure image is in proper orientation, no FITS files are
           ;; written, so this doesn't junk up the header
           bias_im = ssgread(files[i], hdr, eim, ehdr, /BIAS) 
           asize=size(bias_im) & nx=asize[1] & ny=asize[2]
           n_ovrclk[i] = ny

           biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
           if count ne 0 then message, 'ERROR: bias has already been subtracted.  Use ssg_raw_cp to erase file and start again'
           
           if keyword_set(TV) then $
             display, /reuse, bias_im, zoom=zoom

           for di = 0,nx-1 do begin
              med_ovrclk[di,i] = median(bias_im[di,*])
              av_ovrclk[di,i] = mean(bias_im[di,*], /NAN)
           endfor
           med_bias[i] = median(bias_im)
           av_bias[i] = mean(bias_im, /NAN)
           stdev_bias[i] = stddev(bias_im, /NAN)

           zeros =where(av_ovrclk eq 0)

           if keyword_set(showplots) then begin
              wset,7
              plot, med_ovrclk[*,i], psym=plus, xrange=[0,nx], $
                    yrange=[min([med_ovrclk,av_ovrclk], /NAN), $
                            max([med_ovrclk,av_ovrclk], /NAN)],$
                    xstyle=2, ystyle=2, $
                    xtitle='Pixel (dispersion direction)', $
                    ytitle='Overlclock value (DN)', $
                    title=files[i]
              
              oplot, av_ovrclk[*,i], psym=diamond
           endif
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL

     if keyword_set(showplots) then begin
        wset,6
        plot,[0,0], xrange=[0,nx], $
             yrange=[min([med_ovrclk,av_ovrclk], /NAN), $
                     max([med_ovrclk, av_ovrclk], /NAN)],$
             xtitle='Pixel (dispersion direction)', $
             ytitle='Bias value (DN)'
        al_legend, ['Median', 'Average'], psym=[plus, diamond], pos=pos, /norm
        for i=0,nf-1 do begin
           oplot, med_ovrclk[*,i], psym=plus
           oplot, av_ovrclk[*,i], psym=diamond
        endfor ;; all files in directory
     endif

  endif ;; not reviewing


  if NOT keyword_set(noninteractive) then begin
     marked_ndays = ssg_mark_bad(ndays, [[med_bias], [av_bias]], $
                                 title=string('Overclock info for files in ', indir), $
                                 xtickunits='Hours', $
                                 xtitle=string('UT time (Hours) ', utdate), $
                                 ytitle='Overlclock value (DN)', $
                                 legend = ['Median', 'Average'], $
                                 window = 3, /reuse, /MJD)
     
     dbclose

     bad_idx = where(finite(marked_ndays) eq 0, count)

     if count gt 0 then badarray[bad_idx] = badarray[bad_idx] OR 8192

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
     dbupdate, entries, 'bad, n_ovrclk, med_ovrclk, av_ovrclk, med_bias, av_bias', badarray, n_ovrclk, med_ovrclk, av_ovrclk, med_bias, av_bias
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated overclock and bad assignment stuff in ' + dbname
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
