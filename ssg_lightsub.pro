;+
; $Id: ssg_lightsub.pro,v 1.2 2003/04/11 19:09:34 jpmorgen Exp $
;-

;; ssg_lightsub Calculates the background spectrum from room lights or
;; whatever, for each image using the rows above and below the slicer
;; image.  Stores spectra in database and subtracts them off of each image

pro ssg_lightsub, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, pos=pos, write=write
  ON_ERROR, 0
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode>1", $
                   dbfind("bad<8191", $ ; < is really <=
                          dbfind(string("dir=", indir))))
  dbext, entries, 'fname, m_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, m_cam_rot, cam_rot', $
         files, m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, m_cam_rots, cam_rots
  dbext, entries, 'edge_coef, edge_coef_err, med_back, av_back, stdev_back', $
         edge_coefs, edge_coef_errs, med_back, av_back, stdev_back

  dbclose

  files=strtrim(files)
  nf = N_elements(files)

  err = 0
  for i=0,nf-1 do begin
;     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; Read in the data section for calculations but the full
        ;; section for rewriting (see below)
        im = ssgread(files[i], hdr, eim, ehdr, /DATA, /TRIM)
        bim = ssgread(files[i], hdr, beim)
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'ERROR: you must run ssg_biassub first'

        junk = strtrim(sxpar(hdr,'MEDBACK',COUNT=count))
        if count ne 0 then message, 'ERROR: background light has already been subtracted.  Database value for median of the background light is ' + string(med_back[i]) + '.  Header has value of ' + string(junk)


        asize = size(im) & nx = asize(1) & ny = asize(2)
        ;; This is the reference pixel in the dispersion direction
        ref_pixel = nx/2.

        ;; Try to get rid of the tail of the slicer pattern
        sli_bots[i] = max([0, floor(sli_bots[i])-2])
        sli_tops[i] = min([ny-1, ceil(sli_tops[i]) + 2])


        rot_im = ssg_camrot(im, -cam_rots[i], ref_pixel, sli_cents[i])

        edge_ny = ny-(sli_tops[i]-sli_bots[i])
        edge_im = fltarr(nx,edge_ny)
        edge_im[*,0:sli_bots[i]-1] = rot_im[*,0:sli_bots[i]-1]
        edge_im[*,sli_bots[i]:ny-(sli_tops[i]-sli_bots[i])-1] = $
          rot_im[*,sli_tops[i]:ny-1]

        ;; I hope it doesn't matter, but since I have the errors
        ;; around...
        rot_eim = ssg_camrot(eim, -cam_rots[i], ref_pixel, sli_cents[i])
        edge_eim = fltarr(nx,edge_ny)
        edge_eim[*,0:sli_bots[i]-1] = rot_eim[*,0:sli_bots[i]-1]
        edge_eim[*,sli_bots[i]:ny-(sli_tops[i]-sli_bots[i])-1] = $
          rot_eim[*,sli_tops[i]:ny-1]

        if keyword_set(TV) then $
          display, edge_im, /reuse

        ;; The background light seems to be a tilted plane.  Fit a
        ;; line to each column and store the results.  Make the
        ;; reference pixel in the cross-dispersion direction the
        ;; boundary between the upper and lower parts in edge_im
        xd_axis = indgen(edge_ny)-sli_bots[i]
        xdisp = fltarr(edge_ny)
        xdisp_err = xdisp
        edge_fit = fltarr(nx, 2)
        edge_fit[*] = !values.f_nan
        edge_fit_err = edge_fit
        for ix=0,nx-1 do begin
           xdisp[*] = edge_im[ix,*]
           xdisp_err[*] = edge_eim[ix,*]
           good_idx = where(finite(xdisp) eq 1 and $
                            finite(xdisp_err) eq 1, count)
           if count gt 0 then begin
              coefs = poly_fit(xd_axis[good_idx], xdisp[good_idx], $
                               1, measure_errors=xdisp_err[good_idx], $
                               sigma=sigma)
              edge_fit[ix,good_idx] = coefs[good_idx]
              edge_fit_err[ix,good_idx] = sigma[good_idx]
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
                               1, measure_errors=coef_err[good_idx], $
                            sigma=sigma)
              edge_coefs[i,ic,good_idx] = coefs[good_idx]
              edge_coef_errs[i,ic,good_idx] = sigma[good_idx]
           endif
        endfor

        window,2
        plot, edge_fit[*,0]
        oplot, poly(xaxis,edge_coefs[i,0,*])
        stop
        plot, edge_fit[*,1]
        oplot, poly(xaxis,edge_coefs[i,1,*])
        stop

        template = template_create(im, edge_spec)
        err_temp = template_create(im, edge_err)

        ;; We want to subtract the background from everything except
        ;; the overclock region
        sxaddhist, string('(ssg_lightsub.pro) ', systime(/UTC), ' UT '), hdr
        sxaddhist, string('(ssg_lightsub.pro) Subtracted planar fit to background light'), hdr
        sxaddhist, string('(ssg_lightsub.pro) measured from edge of image.  Overclock not touched'), hdr
        im = im - template
        eim = sqrt(eim^2 + err_temp^2)

        bim[0:nx-1,0:ny-1] = im[*,*]
        beim[0:nx-1,0:ny-1] = eim[*,*]

        med_back[i] 	= median(edge_im)
        av_back[i] 	= mean(edge_im, /NAN)
        stdev_back[i] 	= stddev(edge_im, /NAN)

        sxaddpar, hdr, 'MEDBACK', med_back[i], 'Median value of measured background light (BUNIT)'
        sxaddpar, hdr, 'AVBACK', av_back[i], 'Average value of measured background light (BUNIT)'
        sxaddpar, hdr, 'STDBACK', stdev_back[i], 'Stdev value of measured background light (BUNIT)'

        ssgwrite, files[i], bim, hdr, beim, ehdr
     endelse
     
  endfor
  CATCH, /CANCEL

  if NOT keyword_set(write) then begin
     for ki = 0,1000 do flush_input = get_kbrd(0)
     repeat begin
        message, /CONTINUE, 'Write these values to the database?([Y]/N)'
        answer = get_kbrd(1)
        if byte(answer) eq 10 then answer = 'Y'
        answer = strupcase(answer)
     endrep until answer eq 'Y' or answer eq 'N'
     for ki = 0,1000 do flush_input = get_kbrd(0)
     if answer eq 'Y' then write=1
  endif
  
  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'edge_coef, edge_coef_err, med_back, av_back, stdev_back', $
               edge_coefs, edge_coef_errs, med_back, av_back, stdev_back
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated background light stuff in ' + dbname
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
