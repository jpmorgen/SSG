;+
; $Id: ssg_biasgen.pro,v 1.9 2015/03/04 15:52:22 jpmorgen Exp $

; ssg_biasgen Generate a bestbias frame from all the bias images in a
; given directory

;-
pro ssg_biasgen, indir, outname, plot=plot, TV=tv, sigma_cut=cutval, badcols=badcols, badrows=badrows
;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(outname) then outname = 'bestbias.fits'
  ;; Be a little restrictive, since pixels can be replaced from the stack
  if NOT keyword_set(cutval) then cutval = 4

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode=0", $
                   dbfind("bad<16383", $ ; < is really <=
                          dbfind(string("dir=", indir), /fullstring)), count=count)
  if count eq 0 then $
     message, 'ERROR: no biases found in ' + indir
  
  dbext, entries, "fname, med_bias, av_bias, stdev_bias, bad", files, med_biases, av_biases, stdev_biases, badarray
  dbclose

  files=strtrim(files)
  nf = N_elements(files)
  night_av = mean(av_biases)

  ;; Read in an image to set up the bestbias header
  im = ssgread(files[0], hdr, eim, ehdr)
  sxaddhist, string('(ssg_biasgen.pro) ', systime(/UTC), ' UT'), hdr

  asize=size(im) & nx=asize[1] & ny=asize[2]

  if keyword_set(badcols) then begin
     bad_idx=where(badcols ge nx, count)
     if count gt 0 then $
       message, 'ERROR: column out of range'
  endif
  if keyword_set(badrows) then begin
     bad_idx=where(badrows ge ny, count)
     if count gt 0 then $
       message, 'ERROR: column out of range'
  endif

  av_im = fltarr(nx,ny)
  count_im = fltarr(nx,ny)
  stack_im = fltarr(nf,nx,ny)
  stack_im[*] = !values.f_nan
  cd,current=cwd
  sxaddpar, hdr, 'PARENTH', getenv("HOST"), ' hostname'
  sxaddpar, hdr, 'PARENTD', cwd, ' current working directory'
  sxaddpar, hdr, 'PARENT', files[0], ' first bias file in directory'
  sxaddpar, hdr, 'RAWFILE', 'NONE', ' Product of reduced files in PARENTD'
  sxaddpar, hdr, 'SSGFILE', outname, ' Name of this file'

  if keyword_set(TV) then display, av_im, hdr, title = 'Single bias image'
  if keyword_set(plot) then window, 6, title='Bias histogram'

  err=0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], ihdr, eim, ehdr)

        ;; Nuke pre-defined bad columns and rows
        for ic=0,N_elements(badcols)-1 do begin
           im[badcols[ic],*] = !values.f_nan
        endfor
        for ir=0,N_elements(badrows)-1 do begin
           im[*,badrows[ir]] = !values.f_nan
        endfor

        if N_elements(av_im) ne N_elements(im) then $
          message, 'ERROR: ' + files[i] + ' and ' + files[0] + ' are not the same size'

        if keyword_set(TV) then begin
           display, im, ihdr, /reuse
           wait, 0.25
        endif

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
        
        ;; Try doing this iteratively so bad columns are caught.  This
        ;; gets a very clean image, but takes away too much and does
        ;; not give clean column breaks
        nbad = nx*ny
        tmp_im = im
        repeat begin
           last_nbad = nbad
           sigma_im = (im-data_med)/stdev 
           mask_im = mark_bad_pix(sigma_im, cutval=cutval)
           ;; Save off just the number of times pixels are identified
           ;; as bad and use the for bad column identification
           bad_count_im = mask_im
           badidx = where(mask_im gt 0, nbad)
           if nbad gt 0 then mask_im[badidx] = !values.f_nan
           tmp_im = im+mask_im
           stdev = stddev(tmp_im, /NAN)
           data_med = median(tmp_im)
        endrep until nbad ge last_nbad
        
        ;; Make up a bad column spotting algorithm
        ssg_spec_extract, bad_count_im, ihdr, colspec, rowspec, /total
        bad_col = where(colspec gt 0, count)
        if count gt 0 then begin
           mask_im[bad_col,*] = !values.f_nan
        endif
        ;;plot, colspec
        ;;stop
        ;;wait, 1
        
        ;;; ;; I find using the bad pixels found in this way is too
        ;;; ;; draconian.  But data_med should be really good + we can use
        ;;; ;; that as a cut pixel-by-pixel
        ;;; badidx = where(im gt data_med + 5.*stdev, count)
        ;;; mask_im = mask_im*0.
        ;;; mask_im[badidx] = !values.f_nan
        

        if keyword_set(TV) then begin
           display, im+mask_im, ihdr, /reuse
           wait, 0.25
        endif

        mask_im = mask_im + 1   ; used multiplicatively below

;        ;; Make sure not too much of the array is contaminated 
;        if nbad gt 0.01*N_elements(mask_im) then begin
;           badarray[i] = badarray[i] OR 16384
;           ;; This message is caught by the code above, skipping the
;           ;; code below.
;           message, 'WARNING: Bias ' + files[i] + ' has more than 1% bright pixels.  Marking it bad in the database'
;        endif

        ;; Show histogram if user wants it
        if keyword_set(plot) then begin
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
;         good_idx = where(finite(sigma_im) eq 0, count)
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
;;        badidx = where(mask_im gt 1, count)
;        if count gt badpix then $
;          message, 'ERROR: too many bad pixels: ' + string(count)
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
  endfor ;; each bias file
  CATCH, /CANCEL

  if total(count_im) eq 0 then message, 'ERROR: no good bias frames found in ' + indir + ' file ' + outname + ' not written, database ' + dbname + ' not modified.  Did you run ssg_get_overclock?'

  ;; Create average bias image
  av_im = av_im / count_im
  sxaddhist, string('(ssg_biasgen.pro) divided by good pixel counting image'), hdr

  message, /INFORMATIONAL, 'Combining images...expect delay'

  ;; Create median bias image and error array.  The error here is
  ;; actually a subtle point.  I think the stddev taken in this way is
  ;; a measure of the uncertainty on a single bias frame.  Since each
  ;; image has a single measurement of the bias in it, this is the
  ;; appropriate error for its subtraction.  The flatfield, on the
  ;; other hand is measuring total electrons collected in a stack of
  ;; images, so we want Poisson statistics there.  Also, the bias
  ;; value is in DN, with some other statistical determination
  ;; (e.g. Johnson noise on the amplifier resistors, or something).
  ;; In the absense of a good independent measure like Poisson
  ;; statistics with a known parent population, the stddev is a
  ;; reasonable approximation
  med_im=fltarr(nx,ny)
  med_im[*] = !values.f_nan
  eim = med_im
  for i=0,nx-1 do begin
     for j=0,ny-1 do begin
        good_idx = where(finite(stack_im[*,i,j]) eq 1, count)
        if count gt 1 then begin
           med_im[i,j] = median(stack_im[*,i,j])
           eim[i,j] = stddev(stack_im[*,i,j], /NAN)
        endif
     endfor
  endfor

  ;;display, med_im, hdr, title = 'Median bias image'

  ;; Replace any remaining suspect pixels in the average image with
  ;; median values

  badidx = where(abs(med_im - av_im) gt stdev, count)
  if count gt 0 then begin
     av_im[badidx] = med_im[badidx]
     message, /INFORMATIONAL, 'cleaned up ' + string(count) + ' pixels using median of all bias images'
     sxaddhist, string('(ssg_biasgen.pro) cleaned up ', count, ' pixels'), hdr
  endif

  ;; For 1996-7 spectra, make a dispersion spectrum to improve
  ;; statistics on finding marginal columns.
  ssg_spec_extract, av_im, hdr, spec, xdisp, /total

  bad_idx = where(abs((spec-median(spec))/stddev(spec, /NAN)) gt cutval, count)
  if count gt 0 then begin
     message, /CONTINUE, 'WARNING: ' + string(count) + ' additional bad columns found.  Setting them to NAN'
     av_im[bad_idx,*] = !values.f_nan
  endif

  ;; For the heck of it, do rows too.
  ssg_spec_extract, av_im, hdr, spec, xdisp, /total
  bad_idx = where(abs((xdisp-median(xdisp))/stddev(xdisp, /NAN)) gt cutval, count)
  if count gt 0 then begin
     message, /CONTINUE, 'WARNING: ' + string(count) + ' additional bad rows found.  Setting them to NAN'
     av_im[*,bad_idx] = !values.f_nan
  endif

  ;; And iterate
  ssg_spec_extract, av_im, hdr, spec, xdisp, /total

  bad_idx = where(abs((spec-median(spec))/stddev(spec, /NAN)) gt cutval, count)
  if count gt 0 then begin
     message, /CONTINUE, 'WARNING: ' + string(count) + ' additional bad columns found.  Setting them to NAN'
     av_im[bad_idx,*] = !values.f_nan
  endif


  hist = histogram(av_im, binsize=1, /NAN)
  if keyword_set(plot) then begin
     window, 6, title='Best bias histogram'
     plot, indgen(N_elements(hist)) + min(av_im, /NAN), $
           hist/float(N_elements(av_im)), $
           title='Best bias histgram', xtitle='bin (DN)', $
           ytitle='fraction of image area'
  endif
  if keyword_set(tv) then begin
     display, av_im, hdr, title = 'Best bias image'
  endif

;  badidx = where(finite(av_im) eq 0, av_im_count)
;  badidx = where(finite(data_im) eq 0, data_im_count)
;  if av_im_count ne data_im_count then message, 'ERROR: ' + string(count) + ' bad pixels not present in input images were found in ' + outname


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

  entries = dbfind(string("dir=", indir), /fullstring)
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
