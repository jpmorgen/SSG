;+
; $Id: ssg_get_overclock.pro,v 1.1 2002/10/28 17:37:13 jpmorgen Exp $

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
  dbext, entries, "fname, nday, date", files, ndays, dates
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  
  files=strtrim(files)

  err=0
  n_ovrclk = intarr(nf)
  med_ovrclk = fltarr(800,nf)
  av_ovrclk = fltarr(800,nf)
  pred_ovrclk = fltarr(800,nf)
  med_bias = fltarr(nf)
  av_bias = fltarr(nf)
  stdev_bias = fltarr(nf)

  med_ovrclk [*] = !values.f_nan
  av_ovrclk  [*] = !values.f_nan
  pred_ovrclk[*] = !values.f_nan

  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], hdr) ; Make sure image is in proper orientation
        biassec = strtrim(sxpar(hdr,'BIASSEC',COUNT=count))
        if count eq 0 then message, 'ERROR: no BIASSEC keyword in FITS header'
        toks=strsplit(biassec,'[:,]')
        if N_elements(toks) ne 4 then message, 'ERROR: unknown BIASSEC string format'
        coords=strsplit(biassec,'[:,]',/extract)
        coords = coords - 1     ; Translate into IDL array reference
        if keyword_set(TV) then begin
           ocr = im[coords[0]:coords[1], coords[2]:coords[3]]
           display, /reuse, ocr, zoom=zoom
        endif
        n_ovrclk[i] = coords[3]-coords[2]
        for di = coords[0], coords[1] do begin
           med_ovrclk[di,i] = median(im[di,coords[2]:coords[3]])
           av_ovrclk[di,i] = total(im[di,coords[2]:coords[3]])/float(n_ovrclk[i])
        endfor
        med_bias[i] = median(im[coords[0]:coords[1], coords[2]:coords[3]])
        av_bias[i] = total(im[coords[0]:coords[1], coords[2]:coords[3]])/ $
          ((coords[1]-coords[0])*float(n_ovrclk[i]))
        stdev_bias[i] = stddev(im[coords[0]:coords[1], coords[2]:coords[3]],/NAN)
        if keyword_set(showplots) then begin
           window,2
           plot, med_ovrclk[*,i], psym=plus, xrange=[coords[0],coords[1]], $
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
     window,2
     plot,[0,0], xrange=[coords[0],coords[1]], $
          yrange=[min([med_ovrclk,av_ovrclk], /NAN), $
                  max([med_ovrclk, av_ovrclk], /NAN)],$
          xtitle='Pixel (dispersion direction)', $
          ytitle='Bias value (DN)'
     legend, ['Median', 'Average'], psym=[plus, diamond], pos=pos, /norm
  endif

  xaxis=indgen(coords[1]-coords[0]) + coords[0]
  for i=0,nf-1 do begin
     oplot, med_ovrclk[*,i], psym=plus
     oplot, av_ovrclk[*,i], psym=diamond
  endfor ;; all files in directory

  window,3
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
