;+
; $Id: ssg_biasgen.pro,v 1.5 2003/03/10 18:27:02 jpmorgen Exp $

; ssg_biasgen Generate a bestbias frame from all the bias images in a
; given directory

;-
pro ssg_biasgen, indir, outname, showplots=showplots, TV=tv, badpix=badpix, sigma_cut=cutval
;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(outname) then outname = 'bestbias.fits'
  if NOT keyword_set(badpix) then badpix = 5

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode=0", $
                   dbfind("bad<16383", $ ; < is really <=
                          dbfind(string("dir=", indir))))
  dbext, entries, "fname, med_bias, av_bias, stdev_bias, bad", files, med_biases, av_biases, stdev_biases, badarray
  dbclose

  files=strtrim(files)
  nf = N_elements(files)
  night_av = mean(av_biases)

  ;; Read in an image to set up the bestbias header
  im = ssgread(files[0], hdr, eim, ehdr)
  sxaddhist, string('(ssg_biasgen.pro) ', systime(/UTC), ' UT'), hdr

  asize=size(im) & nx=asize[1] & ny=asize[2]
  av_im = fltarr(nx,ny)
  count_im = fltarr(nx,ny)
  stack_im = fltarr(nf,nx,ny)
  cd,current=cwd
  sxaddpar, hdr, 'PARENTH', getenv("HOST"), ' hostname'
  sxaddpar, hdr, 'PARENTD', cwd, ' current working directory'
  sxaddpar, hdr, 'PARENT', files[0], ' first bias file in directory'
  sxaddpar, hdr, 'RAWFILE', 'NONE', ' Product of reduced files in PARENTD'
  sxaddpar, hdr, 'SSGFILE', outname, ' Name of this file'

  if keyword_set(TV) then display, av_im, hdr, title = 'Single bias image'
  if keyword_set(showplots) then window, 6, title='Bias histogram'

  err=0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], ihdr, eim, ehdr)

        if N_elements(av_im) ne N_elements(im) then $
          message, 'ERROR: ' + files[i] + ' and ' + files[0] + ' are not the same size'

        if keyword_set(TV) then display, im, ihdr, /reuse

        ;; Just in case there was a light leak or something, calculate
        ;; the best bias value from the data section separately from
        ;; the overclock section.
        data_im = im
        data_med=median(data_im)
        stdev=stddev(data_im, /NAN)    ; This will be improved below
        if (abs(data_med - med_biases[i]) gt $
            (stdev + stdev_biases[i])) then begin
           message, /CONTINUE, 'WARNING: Bias ' + files[i] + ' image area median is ' + string(data_med) + '+/-' + string(stdev) + ' median of overclock is ' + string(med_biases[i]) + '+/-' + string(stdev_biases[i]) + ' Checking average overclock value'
           if (abs(data_med - av_biases[i]) gt $
               (stdev + stdev_biases[i])) then begin
              badarray[i] = badarray[i] OR 16384
              ;; This message is caught by the code above, skipping
              ;; the code below.
              message, 'WARNING: Bias ' + files[i] + ' image area median is ' + string(data_med) + '+/-' + string(stdev) + ' average of overclock is ' + string(med_biases[i]) + '+/-' + string(stdev_biases[i]) + ' The difference is too great, so I will mark this image as bad in the database'
           endif
        endif

        ;; Now work with the whole image
        sigma_im = (im-data_med)/stdev ; could use template_statistic but stdev already calculated here
        mask_im = mark_bad_pix(sigma_im, cutval=cutval)
        badidx = where(mask_im gt 0, count)
        if count gt 0 then mask_im[badidx] = !values.f_nan
        if keyword_set(TV) then display, im*mask_im, ihdr, /reuse
        mask_im = mask_im + 1   ; used multiplicatively below

        ;; Make sure not too much of the array is contaminated 
        if count gt 0.01*N_elements(mask_im) then begin
           badarray[i] = badarray[i] OR 16384
           ;; This message is caught by the code above, skipping the
           ;; code below.
           message, 'WARNING: Bias ' + files[i] + ' has more than 1% bright pixels.  Marking it bad in the database'
        endif

        ;; Show histogram if user wants it
        if keyword_set(showplots) then begin
           hist = histogram(sigma_im, min=-10, max=10, binsize=1, $
                            reverse_indices=R, /NAN)
           nh = N_elements(hist)
           wset, 6
           plot, indgen(nh) - nh/2,hist
        endif

;; Useful code for something later on
;         ;; Mark bad pixels using reverse lookup of histgram.  If the
;         ;; bias measurements are a good Gaussian, this cuts things off
;         ;; at the 5-sigma point.  However, do it this way just in case
;         ;; there is a non-Gaussian distribution
;         good_idx = where(finite(sigma_im), count)
;         for hi = 0, N_elements(hist)-1 do $
;           if hist[hi] gt 0 and hist[hi] lt 1.5E-6*count then $
;           mask_im[R[R[hi] : R[hi+1]-1]] = !values.f_nan

        ;; If we made it this far, the image is good except for maybe
        ;; a few pixels.

        ;; Check to see if the average of the cleaned up image is off
        ;; from the nightly average by more than 1 sigma.  Set it to
        ;; the nightly average since that is what we are going to do
        ;; with the real data

        delta = av_biases[i] - night_av
        stdev = stddev(im*mask_im, /NAN)
        if stdev_biases[i] lt abs(delta) and stdev lt abs(delta) then begin
           im = im - delta
           sxaddhist, string(format='("(ssg_biasgen.pro) shifted ", a, " by", f6.3, " DN")', files[i], -delta), hdr
           message, /CONTINUE, 'WARNING: bias '+ files[i] + ' is off by ' + string(delta) + ' DN from the average bias level for this night.  Adjusting...'
        endif
        

        ;; Average: exclude bad pixels from average calculation by
        ;; setting them to 0 before adding AND keeping track of how
        ;; many good pixels were collected at each location
        badidx = where(finite(mask_im) eq 0, count)
        if count gt badpix then $
          message, 'ERROR: too many bad pixels: ' + string(count)
        count_im = count_im + 1
        if count gt 0 then begin 
           im[badidx] = 0.
           count_im[badidx] = count_im[badidx] - 1
        endif
        av_im = av_im + im
        sxaddhist, string(format='("(ssg_biasgen.pro) added ", a, i6, " bad pixels")', files[i], count), hdr
        message, /INFORMATIONAL, 'added ' + files[i] +  string(count) + ' bad pixels'

        ;;  Array for median
        stack_im[i,*,*] = im[*,*]*mask_im
        
     endelse
  endfor
  CATCH, /CANCEL

  if total(count_im) eq 0 then message, 'ERROR: no good bias frames found in ' + indir + ' file ' + outname + ' not written, database ' + dbname + ' not modified.  Did you run ssg_get_overclock?'

  ;; Create average bias image
  av_im = av_im / count_im
  sxaddhist, string('(ssg_biasgen.pro) divided by good pixel counting image'), hdr

  ;; Create median bias image and error array.  Mark missing pixels
  ;; with NAN so median and stddev ignore them.  The error here is
  ;; actually a subtle point.  I think the stddev taken in this way
  ;; is a measure of the uncertainty on a single bias frame.  Since
  ;; each image has a single measurement of the bias in it, this is
  ;; the appropriate error for its subtraction.  The flatfield, on the
  ;; other hand is measuring total electrons collected in a stack of
  ;; images, so we want Poisson statistics there.  Also, the bias
  ;; value is in DN, with some other statistical determination
  ;; (e.g. Johnson noise on the amplifier resistors, or something).
  ;; In the absense of a good independent measure like Poisson
  ;; statistics with a known parent population, the stddev is a
  ;; reasonable approximation
  bad_idx = where(stack_im eq 0, count)
  if count gt 0 then $
    stack_im[bad_idx] = !values.f_nan
  med_im=fltarr(nx,ny)
  for i=0,nx-1 do begin
     for j=0,ny-1 do begin
        med_im[i,j] = median(stack_im[*,i,j])
        eim[i,j] = stddev(stack_im[*,i,j], /NAN)
     endfor
  endfor

  ;;display, med_im, hdr, title = 'Median bias image'

  ;; Replace any remaining suspect pixels in the average image with
  ;; median values

  print, median(med_im), median(av_im)
  badidx = where(abs(med_im - av_im) gt stdev, count)
  if count gt 0 then begin
     av_im[badidx] = med_im[badidx]
     message, /INFORMATIONAL, 'cleaned up ' + string(count) + ' pixels using median of all bias images'
     sxaddhist, string('(ssg_biasgen.pro) cleaned up ', count, ' pixels'), hdr
  endif

  hist = histogram(av_im, binsize=1, /NAN)
  window, 6, title='Best bias histogram'
  plot, indgen(N_elements(hist)) + min(av_im, /NAN), hist/float(N_elements(av_im)), $
        title='Best bias histgram', xtitle='bin (DN)', $
        ytitle='fraction of image area'
  display, av_im, hdr, title = 'Best bias image'

  badidx = where(finite(av_im) eq 0, av_im_count)
  badidx = where(finite(data_im) eq 0, data_im_count)
  if av_im_count ne data_im_count then message, 'ERROR: ' + string(count) + ' bad pixels not present in input images were found in ' + outname


  ;; Write out the bestbias image
  ssgwrite, outname, av_im, hdr, eim, ehdr
  message, /INFORMATIONAL, 'Wrote file ' + outname

  ;; Update the bad column in the database, if necessary
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  temp=where(badarray gt 0, count)
  if count gt 0 then begin
     dbupdate, entries, 'bad', badarray
     message, /INFORMATIONAL, 'Recorded bad bias image(s) in ' + dbname
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
