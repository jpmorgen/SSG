;+
; $Id: ssg_get_overclock.pro,v 1.2 2002/10/31 22:17:32 jpmorgen Exp $

; ssg_get_overclock.  collects information on the CCD overclock region
; to put into the reduction database.

;-

pro ssg_get_overclock, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, pos=pos

  ON_ERROR, 2
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
  entries = dbfind(string("dir=", indir))
  dbext, entries, "fname, nday, date, n_ovrclk, med_ovrclk, av_ovrclk, pred_ovrclk, med_bias, av_bias, stdev_bias", files, ndays, dates, n_ovrclk, med_ovrclk, av_ovrclk, pred_ovrclk, med_bias, av_bias, stdev_bias

  files=strtrim(files)
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  
  err=0

  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        if n_ovrclk[i] ne 0 then message, /CONTINUE, 'WARNING: bias info already in database'
        ;; Make sure image is in proper orientation, no FITS files are
        ;; written, so this doesn't junk up the header
        im = ssgread(files[i], hdr, /BIAS) 
        asize=size(im)
        nx=asize[1]
        ny=asize[2]
        n_ovrclk[i] = ny

        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count ne 0 then message, 'ERROR: bias has already been subtracted.  Use ssg_raw_cp to erase file and start again'
        
        med_bias   [i]   = !values.f_nan
        av_bias    [i]   = !values.f_nan
        stdev_bias [i]   = !values.f_nan
        med_ovrclk [*,i] = !values.f_nan
        av_ovrclk  [*,i] = !values.f_nan
        pred_ovrclk[*,i] = !values.f_nan

        if keyword_set(TV) then $
          display, /reuse, im, zoom=zoom

        for di = 0,nx-1 do begin
           med_ovrclk[di,i] = median(im[di,*])
           av_ovrclk[di,i] = total(im[di,*])/float(ny)
        endfor
        med_bias[i] = median(im)
        av_bias[i] = total(im)/N_elements(im)
        stdev_bias[i] = stddev(im, /NAN)
        if keyword_set(showplots) then begin
           window,6
           plot, med_ovrclk[*,i], psym=plus, xrange=[0,nx], $
                 yrange=[min([med_ovrclk,av_ovrclk], /NAN), $
                         max([med_ovrclk, av_ovrclk], /NAN)],$
                 xtitle='Pixel (dispersion direction)', $
                 ytitle='Bias value (DN)'
           oplot, av_ovrclk[*,i], psym=diamond
        endif
     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  dbclose

  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'n_ovrclk, med_ovrclk, av_ovrclk, pred_ovrclk, med_bias, av_bias', n_ovrclk, med_ovrclk, av_ovrclk, pred_ovrclk, med_bias, av_bias
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated overclock stuff in ' + dbname

  if keyword_set(showplots) then begin
     window,6
     plot,[0,0], xrange=[0,nx], $
          yrange=[min([med_ovrclk,av_ovrclk], /NAN), $
                  max([med_ovrclk, av_ovrclk], /NAN)],$
          xtitle='Pixel (dispersion direction)', $
          ytitle='Bias value (DN)'
     legend, ['Median', 'Average'], psym=[plus, diamond], pos=pos, /norm
  endif

  xaxis=indgen(nx)
  for i=0,nf-1 do begin
     oplot, med_ovrclk[*,i], psym=plus
     oplot, av_ovrclk[*,i], psym=diamond
  endfor ;; all files in directory

  window,7
  plot, ndays, med_bias, title=string('Overclock info for files in ', indir), $
        xtickunits='Hours', $
        yrange=[min([med_bias,av_bias], /NAN), $
                max([med_bias, av_bias], /NAN)], $
        xstyle=1, ystyle=1, psym=plus, $
        xtitle=string('UT time (Hours) ', utdate), $
        ytitle='Bias value (DN)'
  oplot, ndays, av_bias, psym=diamond

  
  if NOT keyword_set(pos) then pos = [0.5,0.5]
  legend, ['Median', 'Average'], psym=[plus, diamond], pos=pos, /norm

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
