;+
; $Id: ssg_biassub.pro,v 1.1 2002/10/25 21:26:13 jpmorgen Exp $

; ssg_biassub Subtract the best bias image from all the (non-bias)
; files in the directory

;-

pro ssg_biassub, indir

;  ON_ERROR, 2
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
;     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], hdr) ; Make sure image is in proper orientation
        sxaddhist, string('(ssg_biassub.pro) ', systime(/UTC), ' UT'), hdr

        bias=readfits(bias_fnames[i], bhdr, SILENT=silent)

        if N_elements(bias) ne N_elements(im) then $
          message, 'ERROR: ' + bias_fnames[i] + ' and ' + files[i] + ' are not the same size'

        biassec = strtrim(sxpar(bhdr,'BIASSEC',COUNT=count))
        if count eq 0 then message, 'ERROR: no BIASSEC keyword in FITS header'
        toks=strsplit(biassec,'[:,]')
        if N_elements(toks) ne 4 then message, 'ERROR: unknown BIASSEC string format'
        coords=strsplit(biassec,'[:,]',/extract)
        coords = coords - 1     ; Translate into IDL array reference

        av_bias_ocr = total(bias[coords[0]:coords[1], coords[2]:coords[3]])/ $
                      ((coords[1]-coords[0])*(coords[3]-coords[2]))
        stdev_bias_ocr = stddev(im[coords[0]:coords[1], coords[2]:coords[3]],/NAN)

        delta = av_biases[i] - av_bias_ocr
        if abs(delta) gt stdev_biases[i]+stdev_bias_ocr then begin
           im = im - delta
           sxaddhist, string('(ssg_biassub.pro) ', systime(/UTC), ' UT'), hdr
           message, /INFORMATIONAL, 'Shifting '+ files[i] + ' by ' + string(delta) + ' DN to agree with best bias image.'
           sxaddhist, string(format='("(ssg_biassub.pro) shifted by", f6.3, " DN")', -delta), hdr
        endif
        im = im - bias
        sxaddhist, "(ssg_biassub.pro) subtracted file given in BIASNAME keyword", hdr
        sxaddpar, hdr, 'BIASFILE', bias_fnames[i], 'Bias frame subtracted'

        message, /INFORMATIONAL, 'Writing ' + files[i]
        writefits, files[i], im, hdr

     endelse

  endfor
  CATCH, /CANCEL

end
