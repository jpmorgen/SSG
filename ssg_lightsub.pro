;+
; $Id: ssg_lightsub.pro,v 1.5 2015/03/04 15:45:07 jpmorgen Exp $
;-

;; ssg_lightsub Calculates the background spectrum from room lights or
;; whatever, for each image using the rows above and below the slicer
;; image.  Stores spectra in database and subtracts them off of each image

pro ssg_lightsub, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, pos=pos, write=write, order=order
  ON_ERROR, 0
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0

  ;; I am not sure if the wiggles I see in the plots of coeficients
  ;; are real or just leaking light.  Assume by default they are
  ;; leaking light and just go flat in the dispersion direction.
  if NOT keyword_set(order) then order=0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode>1", $
                   dbfind("bad<8191", $ ; < is really <=
                          dbfind(string("dir=", indir), /fullstring)))
  dbext, entries, 'fname, m_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, m_cam_rot, cam_rot', $
         files, m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, m_cam_rots, cam_rots
  dbext, entries, 'edge_coef, edge_coef_err, med_back, av_back, stdev_back', $
         edge_coefs, edge_coef_errs, med_back, av_back, stdev_back

  dbclose

  files=strtrim(files)
  nf = N_elements(files)

  if keyword_set(showplots) then begin
     window,6
  endif

  med_back[*]   = !values.f_nan
  av_back[*]    = !values.f_nan
  stdev_back[*] = !values.f_nan

  err = 0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
     endif else begin
        ;; Read in the data section for calculations but the full
        ;; section for rewriting (see below)
        im = ssgread(files[i], hdr, eim, ehdr, /DATA, /TRIM)
        bim = ssgread(files[i], hdr, beim)
        junk = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'ERROR: you must run ssg_biassub first'

        junk = strtrim(sxpar(hdr,'SLI_CENT',COUNT=count))
        if count eq 0 then message, 'ERROR: run ssg_[get & fit]_sliloc first'

        junk = strtrim(sxpar(hdr,'CAM_ROT',COUNT=count))
        if count eq 0 then message, 'ERROR: run ssg_[get & fit]_camrot first'

        junk = strtrim(sxpar(hdr,'MEDBACK',COUNT=count))
        if count ne 0 then message, 'ERROR: background light has already been subtracted.  Database value for median of the background light is ' + string(med_back[i]) + '.  Header has value of ' + string(junk)


        asize = size(im) & nx = asize(1) & ny = asize(2)
        ;; This is the reference pixel in the dispersion direction
        ref_pixel = nx/2.

        ;; Try to get rid of the tail of the slicer pattern
        sli_bots[i] = max([1, floor(sli_bots[i]) - 1])
        sli_tops[i] = min([ny-2, ceil(sli_tops[i]) + 1])

        ;; Blank out the slicer pattern
        mask = fltarr(nx,ny)+1.
        mask[*,sli_bots[i]:sli_tops[i]] = !values.f_nan
        mask = ssg_camrot(mask, -cam_rots[i], ref_pixel, sli_cents[i])
        
        ;; Check to see if there are any good pixels left.
        good_idx = where(finite(mask) eq 1, count)
        if count eq 0 then begin
           sxaddhist, string('(ssg_lightsub.pro) ', systime(/UTC), ' UT '), hdr
           sxaddpar, hdr, 'MEDBACK', med_back[i], 'Median value of measured background light (BUNIT)'
           sxaddpar, hdr, 'AVBACK', av_back[i], 'Average value of measured background light (BUNIT)'
           sxaddpar, hdr, 'STDBACK', stdev_back[i], 'Stdev value of measured background light (BUNIT)'
           ssgwrite, files[i], bim, hdr, beim, ehdr
           message, 'WARNING: no good pixels found, background light not subtraccted'
        endif ;; No good pixels

        edge_im = im * mask
        edge_eim = eim * mask
        med_back[i] 	= median(edge_im)
        av_back[i] 	= mean(edge_im, /NAN)
        stdev_back[i] 	= stddev(edge_im, /NAN)

        if keyword_set(TV) then begin
           display, edge_im, /reuse
           wait, 1
        endif

        ;; The background light seems to be a tilted plane.  Fit a
        ;; line to each column and store the results.  Make the
        ;; reference pixel in the cross-dispersion direction the
        ;; boundary between the upper and lower parts in edge_im
        xd_axis = indgen(ny)-sli_cents[i]
        xdisp = fltarr(ny)
        xdisp_err = xdisp
        edge_fit = fltarr(nx, 2)
        edge_fit[*] = !values.f_nan
        edge_fit_err = edge_fit
        for ix=0,nx-1 do begin
           xdisp[*] = edge_im[ix,*]
           xdisp_err[*] = edge_eim[ix,*]
           good_idx = where(finite(xdisp) eq 1 and $
                            finite(xdisp_err) eq 1, count)
           if count gt 1 then begin
              coefs = poly_fit(xd_axis[good_idx], xdisp[good_idx], $
                               1, measure_errors=xdisp_err[good_idx], $
                               sigma=sigma)
              edge_fit[ix,*] = coefs[*]
              edge_fit_err[ix,*] = sigma[*]
           endif
        endfor

        ;; Now fit those coefficients as a function of dispersion direction
        xaxis = indgen(nx) - ref_pixel
        coef = fltarr(nx)
        coef_err = coef
        for ic=0,1 do begin
           coef[*] = edge_fit[*,ic]
           coef_err[*] = edge_fit_err[*,ic]
           good_idx = where(finite(coef) eq 1 and $
                            finite(coef_err) eq 1, count)
           if count gt 0 then begin
              coefs = poly_fit(xaxis[good_idx], coef[good_idx], $
                               order, measure_errors=coef_err[good_idx], $
                            sigma=sigma)
              edge_coefs[2*ic:2*ic+order,i] = coefs[*]
              edge_coef_errs[2*ic:2*ic+order,i] = sigma[*]
           endif
        endfor

        m=fltarr(nx)
        m_err2 = m
        b=m
        b_err2=m
        for ic=0,order do begin
           b = b + edge_coefs[ic,i]*xaxis^ic
           b_err2 = b_err2 + (edge_coef_errs[ic,i]*xaxis^ic)^2
           m = m + edge_coefs[ic+2,i]*xaxis^ic
           m_err2 = m_err2 + (edge_coef_errs[ic+2,i]*xaxis^ic)^2
        endfor

        good_idx = where(finite(edge_fit[*,0]) eq 1 $
                         and finite(edge_fit[*,0]) eq 1, count)
        if count eq 0 then begin
           sxaddhist, string('(ssg_lightsub.pro) ', systime(/UTC), ' UT '), hdr
           sxaddpar, hdr, 'MEDBACK', med_back[i], 'Median value of measured background light (BUNIT)'
           sxaddpar, hdr, 'AVBACK', av_back[i], 'Average value of measured background light (BUNIT)'
           sxaddpar, hdr, 'STDBACK', stdev_back[i], 'Stdev value of measured background light (BUNIT)'
           ssgwrite, files[i], bim-med_back[i], hdr, $
                     sqrt(beim^2+stdev_back[i]^2), ehdr
           message, 'WARNING: ' + files[i] + ' Background light is not well behaved, subtracting median value'
        endif
        if keyword_set(showplots) then begin
           wset, 6
           !p.multi=[0,0,2]
           plot, edge_fit[*,0], xtitle='pixels', ytitle='Intercept', $
                 title='Fit to column-by-column background light fit coeficients'
           oplot, b

           plot, edge_fit[*,1], xtitle='pixels', ytitle='Slope'
           oplot, m
           !p.multi=0
        endif

        sim = fltarr(nx,ny)
        s_eim2 = sim
        for ix=0,nx-1 do begin
           sim[ix,*] = b[ix] + m[ix] * xd_axis
           s_eim2[ix,*] = b_err2[ix] + m_err2[ix] * xd_axis
        endfor

        ;; We want to subtract the background from everything except
        ;; the overclock region
        sxaddhist, string('(ssg_lightsub.pro) ', systime(/UTC), ' UT '), hdr
        sxaddhist, string('(ssg_lightsub.pro) Subtracted planar fit to background light'), hdr
        sxaddhist, string('(ssg_lightsub.pro) measured from edge of image.  Overclock not touched'), hdr
        im = im - sim
        eim = sqrt(eim^2 + s_eim2)

        bim[0:nx-1,0:ny-1] = im[*,*]
        beim[0:nx-1,0:ny-1] = eim[*,*]

        sxaddpar, hdr, 'MEDBACK', med_back[i], 'Median value of measured background light (BUNIT)'
        sxaddpar, hdr, 'AVBACK', av_back[i], 'Average value of measured background light (BUNIT)'
        sxaddpar, hdr, 'STDBACK', stdev_back[i], 'Stdev value of measured background light (BUNIT)'

        ssgwrite, files[i], bim, hdr, beim, ehdr
        message, /INFORMATIONAL, 'Wrote ' + files[i]
     endelse
     
  endfor
  CATCH, /CANCEL

  ;; Since we just wrote the files, we might as well write the database.
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'edge_coef, edge_coef_err, med_back, av_back, stdev_back', $
            edge_coefs, edge_coef_errs, med_back, av_back, stdev_back
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated background light stuff in ' + dbname


  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
