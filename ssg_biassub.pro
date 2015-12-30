;+
; $Id: ssg_biassub.pro,v 1.6 2003/06/11 18:08:44 jpmorgen Exp $

; ssg_biassub Subtract the best bias image from all the (non-bias)
; files in the directory

;-

pro ssg_biassub, indir

  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode>1", $
                   dbfind("bad<8191", $ ; < is really <=
                          dbfind(string("dir=", indir), /fullstring)))
  dbext, entries, "fname, med_bias, av_bias, stdev_bias, bad, bias_fname", files, med_biases, av_biases, stdev_biases, badarray, bias_fnames
  dbclose

  files=strtrim(files)
  bias_fnames=strtrim(bias_fnames)
  nf = N_elements(files)

  err = 0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], hdr, eim, ehdr)
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count ne 0 then message, 'ERROR: bias frame ' + biasfile+ ' has already been subtracted'

        sxaddhist, string('(ssg_biassub.pro) ', systime(/UTC), ' UT'), hdr

        ;; Read in bestbias image
        bias=ssgread(bias_fnames[i], bhdr, beim)
        if N_elements(bias) ne N_elements(im) then $
          message, 'ERROR: ' + bias_fnames[i] + ' and ' + files[i] + ' are not the same size'
        
        ;; Use the best bias's overclock region as a standard by which
        ;; to shift all the other file
        bestbias_ocr=ssgread(bias_fnames[i], bhdr, /BIAS)
        ;; There had better not be any NANs in the bestbias OCR, but
        ;; just in case....
        av_bestbias_ocr = mean(bestbias_ocr, /NAN)
        stdev_bestbias_ocr = stddev(bestbias_ocr, /NAN)

        delta = av_biases[i] - av_bestbias_ocr
        ;; Only shift if we have reached some sort of threshold value
        if abs(delta) gt stdev_biases[i]+stdev_bestbias_ocr then begin
           im = im - delta
           message, /INFORMATIONAL, 'Shifting '+ files[i] + ' by ' + string(-delta) + ' DN to agree with ' + bias_fnames[i]
           sxaddhist, string(format='("(ssg_biassub.pro) shifted by ", f6.2, " DN")', -delta), hdr
        endif
        im = im - bias
        eim = beim
        sxaddhist, "(ssg_biassub.pro) subtracted file given in BIASFILE keyword", hdr
        sxaddpar, hdr, 'BIASFILE', bias_fnames[i], 'Bias frame subtracted'
        
        ;; GAIN should be here if ssg_db_init worked.  Should be
        ;; modified by external procs (e.g. ssg_exceptions) if
        ;; determined gain should be different than NSO/KPNO supplied
        ;; value
        gain=sxpar(hdr, 'GAIN', count=count)
        if count eq 0 then begin
           message, /CONTINUE, 'WARNING: GAIN keyword missing'
           detector=sxpar(hdr, 'DETECTOR', count=count)
           if count eq 0 then message, 'ERROR: no DETECTOR keyword, cannot guess GAIN'
           gain = 4.1
           message, /CONTINUE, 'WARNING: Guessing GAIN = ' + string(gain)
           sxaddhist, "(ssg_biassub.pro) estimating GAIN from 1993 TI4 value", hdr
           sxaddpar, hdr, 'GAIN', gain, 'electrons per ADU, ESTIMATE'

        endif
        im = im * gain
        eim = eim * gain
        ;; The statistical error on each data image pixel is the
        ;; Poisson error on the number of electrons in that pixel.
        ;; Add that in quadrature from here on out
        eim = sqrt(eim^2 + im)
        sxaddhist, "(ssg_biassub.pro) Converted DN to electrons using GAIN keyword", hdr
        sxaddpar, hdr, 'BUNIT', 'ELECTRONS', 'Pixel units'
        sxaddhist, "(ssg_biassub.pro) Added statistical error array extension", hdr
        sxaddpar, hdr, 'EXTEND', 'T', 'Error array extension is present'

        message, /INFORMATIONAL, 'Converting to electrons and writing ' + files[i]
        ssgwrite, files[i], im, hdr, eim, ehdr

     endelse

  endfor
  CATCH, /CANCEL

end
