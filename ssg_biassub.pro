;+
; $Id: ssg_biassub.pro,v 1.3 2002/11/21 20:02:26 jpmorgen Exp $

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
                   dbfind("bad<16383", $ ; < is really <=
                          dbfind(string("dir=", indir))))
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
        im = ssgread(files[i], hdr) ; Make sure image is in proper orientation
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count ne 0 then message, 'ERROR: bias frame ' + biasfile+ ' has already been subtracted'

        sxaddhist, string('(ssg_biassub.pro) ', systime(/UTC), ' UT'), hdr

        ;; Read in bestbias image
        bias=ssgread(bias_fnames[i], bhdr)
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
        sxaddhist, "(ssg_biassub.pro) subtracted file given in BIASFILE keyword", hdr
        sxaddpar, hdr, 'BIASFILE', bias_fnames[i], 'Bias frame subtracted'
        
        ;; GAIN should be here if ssg_db_init worked.  Should be
        ;; modified by external procs (e.g. ssg_adj_gain) if determine
        ;; gain should be different than NSO/KPNO supplied value
        gain=sxpar(hdr, 'GAIN')  
        sxaddhist, "(ssg_biassub.pro) Converted DN to electrons using GAIN keyword", hdr
        sxaddpar, hdr, 'BUNIT', 'ELECTRONS', 'Pixel units'

        message, /INFORMATIONAL, 'Converting to electrons and writing ' + files[i]
        writefits, files[i], im, hdr

     endelse

  endfor
  CATCH, /CANCEL

end
