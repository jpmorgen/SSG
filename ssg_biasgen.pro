;+
; $Id: ssg_biasgen.pro,v 1.1 2002/10/25 18:03:45 jpmorgen Exp $

; ssg_biasgen Generate a bestbias frame from all the bias images in a
; given directory

;-
pro ssg_biasgen, indir, outname, showhists=showhists, TV=tv
  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(outname) then outname = 'bestbias.fits'

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode=0", $
                   dbfind("bad<32767", $ ; < is really <=
                          dbfind(string("dir=", indir))))
  dbext, entries, "fname, med_bias, av_bias, stdev_bias, bad", files, med_biases, av_biases, stdev_biases, badarray
  dbclose

  files=strtrim(files)
  nf = N_elements(files)
  night_av = total(av_biases)/nf

  ;; Read in an image to set up the bestbias header
  av_im = ssgread(files[0], hdr)
  av_im[*,*] = 0.
  count_im = av_im
  cd,current=cwd
  sxaddpar, hdr, 'PARENTH', getenv("HOST"), ' hostname'
  sxaddpar, hdr, 'PARENTD', cwd, ' current working directory'
  sxaddpar, hdr, 'PARENT', files[0], ' first bias file in directory'

  if keyword_set(TV) then display, av_im, hdr, title = 'Single bias image'
  if keyword_set(showhists) then window, 6, title='Bias histogram'

  err=0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], ihdr) ; Make sure image is in proper orientation
        if keyword_set(TV) then display, im, ihdr, /reuse

        ;; Get non-overclock region coordinates
        trimsec = strtrim(sxpar(ihdr,'TRIMSEC',COUNT=count))
        if count eq 0 then message, 'ERROR: no TRIMSEC keyword in FITS header'
        toks=strsplit(trimsec,'[:,]')
        if N_elements(toks) ne 4 then message, 'ERROR: unknown TRIMSEC string format'
        coords=strsplit(trimsec,'[:,]',/extract)
        coords = coords - 1     ; Translate into IDL array reference
        
        ;; Just in case there was a light leak or something, calculate
        ;; the best bias value from the trim section separately from
        ;; the overclock section.
        trim_im = im[coords[0]:coords[1],coords[2]:coords[3]]
        trim_med=median(trim_im)
        stdev=stddev(trim_im)    ; This will be improved below
        if (abs(trim_med - med_biases[i]) gt $
            (stdev + stdev_biases[i])) then begin
           message, /CONTINUE, 'WARNING: Bias ' + files[i] + ' image area median is ' + string(trim_med) + '+/-' + string(stdev) + ' median of overclock is ' + string(med_biases[i]) + '+/-' + string(stdev_biases[i]) + ' Checking average overclock value'
           if (abs(trim_med - av_biases[i]) gt $
               (stdev + stdev_biases[i])) then begin
              badarray[i] = badarray[i] + 32768
              ;; This message is caught by the code above, skipping
              ;; the code below.
              message, 'WARNING: Bias ' + files[i] + ' image area median is ' + string(trim_med) + '+/-' + string(stdev) + ' average of overclock is ' + string(med_biases[i]) + '+/-' + string(stdev_biases[i]) + ' The difference is too great, so I will mark this image as bad in the database'
           endif
        endif

        ;; Now work with the whole image
        sigma_im = (im-trim_med)/stdev ; could use template_statistic but stdev already calculated here
        mask_im = im
        mask_im[*,*] = 1.
        badidx = where(abs(sigma_im) gt 10, count)        
        if count gt 0 then mask_im[badidx] = !values.f_nan

        ;; Make sure not too much of the array is contaminated at the
        ;; 10 sigma level
        if count gt 0.01*N_elements(mask_im) then begin
           badarray[i] = badarray[i] + 32768
           ;; This message is caught by the code above, skipping the
           ;; code below.
           message, 'WARNING: Bias ' + files[i] + ' has more than 1% bright pixels.  Marking it bad in the database'
        endif

        ;; Make a histogram
        hist = histogram(sigma_im, min=-10, max=10, binsize=1, $
                         reverse_indices=R, /NAN)
        if keyword_set(showhists) then begin
           nh = N_elements(hist)
           wset, 6
           plot, indgen(nh) - nh/2,hist
        endif

        ;; Mark bad pixels using reverse lookup of histgram.  If the
        ;; bias measurements are a good Gaussian, this cuts things off
        ;; at the 5-sigma point.  However, do it this way just in case
        ;; there is a non-Gaussian distribution
        good_idx = where(finite(sigma_im), count)
        for hi = 0, N_elements(hist)-1 do $
          if hist[hi] gt 0 and hist[hi] lt 1.5E-6*count then $
          mask_im[R[R[hi] : R[hi+1]-1]] = !values.f_nan

        ;; Check to see if the average of the cleaned up image is off
        ;; from the nightly average by more than 1 sigma.  Set it to
        ;; the nightly average since that is what we are going to do
        ;; with the real data
        delta = av_biases[i] - night_av
        stdev = stddev(im*mask_im, /NAN)
        if stdev_biases[i] lt abs(delta) and stdev lt abs(delta) then begin
           im = im - delta
           message, /CONTINUE, 'WARNING: bias '+ files[i] + ' is off by ' + delta + ' DN from the average bias level for this night.  Adjusting...'
        endif
        
        ;; If we made it this far, the image is good.  Will adjust
        ;; count_im pixels below
        count_im = count_im + 1

        ;; Exclude bad pixels from average calculation by setting them
        ;; to 0 before adding AND keeping track of how many good
        ;; pixels were collected at each location
        badidx = where(finite(mask_im) eq 0, count)
        if count gt 0 then begin 
           im[badidx] = 0.
           count_im[badidx] = count_im[badidx] - 1
        endif
        av_im = av_im + im
        sxaddhist, string(format='("(ssg_biasgen.pro) added ", a, i6, " bad pixels")', files[i], count), hdr
        message, /INFORMATIONAL, 'added ' + files[i] +  string(count) + ' bad pixels'
        
     endelse
  endfor
  CATCH, /CANCEL

  if total(count_im) eq 0 then message, 'ERROR: no good bias frames found in ' + indir + ' file ' + outname + ' not written, database ' + dbname + ' not modified.  Did you run ssg_get_overclock?'

  ;; Display best bias image 
  av_im = av_im / count_im
  sxaddhist, string('(ssg_biasgen.pro) divided by good pixel counting image'), hdr
  hist = histogram(av_im, binsize=1, /NAN)
  window, 6, title='Best bias histogram'
  plot, hist
  display, av_im, hdr, title = 'Best bias image'

  badidx = where(finite(av_im) eq 0, count)
  if count gt 0 then message, 'ERROR: ' + string(count) + ' bad pixels found in ' + outname


  ;; Write out the bestbias image
  writefits, outname, av_im, hdr
  message, /INFORMATIONAL, 'Wrote file ' + outname

  ;; Update the bad column in the database, if necessary
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  temp=where(badarray gt 0, count)
  if count gt 0 then begin
     dbupdate, entries, 'bad', badarray
     message, /INFORMATIONAL, 'Recorded bad bias image in ' + dbname
  endif

  entries = dbfind(string("dir=", indir))
  nf = N_elements(entries)
  bias_fnames = strarr(nf)
  bias_fnames[*] = outname
  dbupdate, entries, 'bias_fname', bias_fnames
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated bias_fnames in ' + dbname
  ;; For convenience 
  cd, indir
  message, /INFORMATIONAL, 'Directory is set to ' + indir


end
