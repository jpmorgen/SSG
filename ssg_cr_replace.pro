;+
; $Id: ssg_cr_replace.pro,v 1.1 2003/06/11 18:11:56 jpmorgen Exp jpmorgen $

; ssg_cr_replace  Replace NAN values with best-fit cross-dispersion
; spectrum.  Assumes cross-dispersion spectrum is the same for every
; column, which is not correct for things with spatially varying
; Doppler shifts (e.g. Jupiter)

;-

;; AUTODERIVATIVE must be set to 1 since I don't calculate analytic
;; derivatives (a 3rd parameter)
function scale_xdisp, pix, params, parinfo=parinfo, xdisp=in_xdisp

  xdisp=in_xdisp
  xdisp[pix] = xdisp[pix]*params[0]
;  plot, pix, xdisp[pix]

  return, xdisp[pix]

end



pro ssg_cr_replace, indir, tv=tv, showplots=showplots, min_frac=min_frac, noninteractive=noninteractive, winnum=winnum, FTOL=ftol

;  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0
  ;; sn_imp is a factor over which the s/n^2 ration needs to change in
  ;; order to bump a point off of a column (see below)
  if NOT keyword_set(sn_imp) then sn_imp = 1.1
  ;; column must have at least this fraction of non NAN pixels to be counted
  if NOT keyword_set(min_frac) then min_frac = 0.5
  if NOT keyword_set(winnum) then winnum=2
;  if NOT keyword_set(ftol) then ftol=1E-4
  if NOT keyword_set(ftol) then ftol=1E-6


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
;  entries = dbfind("typecode=[2,5]", $
  entries = dbfind("typecode=5", $
                   dbfind("bad<2047", $ ; < is really <=
                          dbfind(string("dir=", indir))), count=count)
  if count eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: no object spectra recorded on ' + indir + '.  Returning without doing anything'
     dbclose
     return
  endif

  dbext, entries, "fname, nday, date, typecode, bad, nbad, ncr, nbad_col", files, ndays, dates, typecodes, badarray, nbads, ncrs, bad_cols

  files=strtrim(files)
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time

  ngood_files = 0
  err = 0
  for i=0,nf-1 do begin
     ;CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; Eventually I'd like to process everything in place, but
        ;; since replacing cosmic rays is time consuming, I have
        ;; done it this way.
        fname = files[i]
        if typecodes[i] eq 5 then begin
           temp=strmid(files[i], 0, strpos(files[i], 'r.fits'))
           fname=temp+'clean.fits'
           fname2=temp+'flat_check.fits'
        endif
        im = ssgread(fname, hdr, eim, ehdr, /DATA, /TRIM)

        cr_cut = sxpar(hdr, 'CR_CUT', count=count)
        if count eq 0 then $
          message, 'ERROR: You must first run ssg_cr_mark'

        sxaddhist, string('(ssg_cr_replace.pro) ', systime(/UTC), ' UT'), hdr

        im = im + ssg_edge_mask(im, hdr)
        thdr = hdr
        eim = eim + ssg_edge_mask(im, thdr)

        sxaddhist, string('Used edge_mask to set edge pixels to NAN in im and eim'), hdr

        ;; Since I will be working a lot with quadradure sums...
        err_im2 = eim^2
        asize=size(im) & nx=asize[1] & ny=asize[2]

        ;; I have tried a couple of different ways to correct for flux
        ;; missing from cosmic ray hits.  I have arrived at this one
        ;; as the best method that preserves the signal-to-noise of
        ;; the spectrum in the dispersion direction.  If you have good
        ;; spectra, in which the median and the mean are similar, it
        ;; doesn't matter much.  But doing it this way makes a big
        ;; difference for the nominal data.  The idea here is to make
        ;; a cross-dispersion spectrum that is fit to each column.
        ;; Missing pixels are replaced with the fit value and an error
        ;; estimate.  Things can then be added up between sli_bot and
        ;; sli_top for the final spectrum.

        ssg_spec_extract, im, hdr, rough_spec, xdisp, /AVERAGE
        ssg_spec_extract, err_im2, hdr, rough_err2, xdisp_err2, /AVERAGE

        ;; Get the good portion of the cross-dispersion spectrum and
        ;; normalize it.  
        full_xdisp_good_idx = where(finite(xdisp) eq 1 $
                                    and finite(xdisp_err2) eq 1, nxpts, $
                                    complement=bad_idx, ncomplement=nbad)
        ;; Note that the pixel in norm_xdisp are relative to
        ;; full_xdisp_good_idx
        norm_xdisp = normalize(xdisp[full_xdisp_good_idx], factor=factor)
        full_norm_xdisp = xdisp*factor
        norm_xdisp_err2 = xdisp_err2[full_xdisp_good_idx]*factor^2
        full_norm_xdisp_err2 = xdisp_err2*factor^2

        ;; The pixel axis in this case is along the column.  Be
        ;; careful about coordinating the X-axis we pass to mpfitfun
        ;; with norm_xdisp
        to_pass = {xdisp:norm_xdisp}

        ;; Replace cosmic ray hits column by column
        badcols = 0
        for di = 0, nx-1 do begin
           column  =           im[di,full_xdisp_good_idx]
           err_col = sqrt(err_im2[di,full_xdisp_good_idx])
           ;; Do a fit to get a best scaling value for the
           ;; cross-dispersion spectrum.  
           params = [rough_spec[di]]
           ;; Since each column has its own set of NANs, work around
           ;; those...
           col_good_idx = where(finite(column) eq 1 and $
                                finite(err_col) eq 1, count, $
                               complement=col_bad_idx)

           ;; Fitting takes a while, so only bother if there good and
           ;; NAN pixels in the column.  Also beware that since
           ;; ssg_spec_extract rotates the image a little, some of the
           ;; NAN areas will grow creating NANs in rough_spec
           if count gt min_frac*nxpts and $
             count lt nxpts and $
             finite(rough_spec[di]) eq 1 then begin
              params = mpfitfun('scale_xdisp', col_good_idx, $
                                column[col_good_idx], err_col[col_good_idx], $
                                params, FUNCTARGS=to_pass, AUTODERIVATIVE=1, $
                                PARINFO=parinfo, maxiter=maxiter, $
                                BESTNORM=chisq, PERROR=perrors, STATUS=status, $
                                /QUIET, FTOL=ftol)
              if status ge 5 then begin
                 message, /CONTINUE, 'WARNING: bad cross-dispersion fit for column ' + string(di)
                 params = [rough_spec[di]]
                 bad_cols[i] = bad_cols[i] + 1
              endif

              rough_spec[di] = params[0]

              ;; Now make a synthetic column with error bars that we will
              ;; use to replace missing pixels.  I used to do a second
              ;; fit with the norm_disp_errs, but I think that is
              ;; counting the error twice.  This should be sufficient.
              column = rough_spec[di] * norm_xdisp
              err_col2 = column^2 * $
                         (norm_xdisp_err2 / norm_xdisp^2 + $
                         (perrors[0] / rough_spec[di])^2)
              
              im[di,full_xdisp_good_idx[col_bad_idx]] = $
                column[col_bad_idx]
              err_im2[di,full_xdisp_good_idx[col_bad_idx]] = $
                err_col2[col_bad_idx]

           endif else begin ;; either a perfect column or a bad column
              if count ne nxpts then begin
                 ;; A bad column
                 im[di,*] = !values.f_nan
                 err_im2[di,*] = !values.f_nan
              endif
           endelse

        endfor ;; replace cosmic ray hits column by column

        sxaddhist, string('(ssg_cr_replace.pro) Replaced NAN pixels with best-fit xdisp values'), hdr        
        sxaddpar, hdr, 'BAD_COLS', bad_cols[i], 'number of columns that had dubious fits '

        message, /INFORMATIONAL, 'Writing ' + fname
        ssgwrite, fname, im, hdr, eim, ehdr

        if keyword_set(TV) then $
          display, im, title=fname, /reuse

        sxaddhist, string('(ssg_cr_replace.pro) Divided by xdisp spectrum'), hdr        
        norm_xdisp_im = template_create(im, full_norm_xdisp)
        ;; Things have been de-rotated, so there should be no need to
        ;; rotate the template
        fim = im/norm_xdisp_im
        feim = fim*sqrt((eim/im)^2 + $
                        template_create(im, full_norm_xdisp_err2) / $
                        norm_xdisp_im^2)

        message, /INFORMATIONAL, 'Writing ' + fname2
        ssgwrite, fname2, fim, hdr, feim, ehdr

        if keyword_set(TV) then begin
           wait, 0.5
           display, fim, title=fname2, /reuse
        endif

        ngood_files = ngood_files + 1

     endelse ;; CATCH if err
  endfor  ;; All files

  CATCH, /CANCEL

  ;; Now write the information to the database
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'nbad_col', bad_cols

  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated bad column count in ' + dbname


  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end

