;+
; $Id: ssg_get_camrot.pro,v 1.3 2002/12/16 13:39:01 jpmorgen Exp $

; ssg_get_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_get_camrot, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, noninteractive=noninteractive, review=review, write=write, maxiter=maxiter

;  ON_ERROR, 2
  cd, indir
  ;; This really converges fast on decent files.  --> I am not sure
  ;; why the data files take so many iterations, even though they
  ;; aproach their end values in about the same time....
  if NOT keyword_set(maxiter) then maxiter=10  

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
  ;; Get all the files in the directory so we can mark camrot as not
  ;; measured on the ones where we can't measure it.
  entries = dbfind(string("dir=", indir))

;  dbext, entries, "fname, nday, date, typecode, bad, m_cam_rot, m_cam_cent", $
;         files, ndays, dates, typecodes, badarray, m_cam_rots, m_cam_cents
  dbext, entries, "fname, nday, date, typecode, bad, m_sli_cent, sli_cent, m_cam_rot", $
         files, ndays, dates, typecodes, badarray, m_sli_cents, sli_cents, m_cam_rots
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(ndays)     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)
  if NOT keyword_set(review) then begin ; We really want to do all the fitting

;;  correlations = fltarr(nsteps)
     m_cam_rots[*] = !values.f_nan

     if keyword_set(showplots) then window,6

     ngood = 0
     err=0

     for i=0,nf-1 do begin
        message, 'Looking at ' + files[i], /CONTINUE
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           if badarray[i] ge 4096 then message, 'BAD FILE, use display, ssg_spec_extract, and look at the header if you are unsure why'
           if typecodes[i] lt 3 then message, 'Can''t get a good camera rotation measurement from bias, dark, or comp images'
;;        if typecodes[i] eq 5 then message, 'Camera parameters can in principle be measured from object spectra, but there seem to be other effects I don''t understand causing variation.  So let''s skip them for now.'

           im = ssgread(files[i], hdr, /DATA)
           biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
           if count eq 0 then message, 'WARNING: works better if you call ssg_biassub first', /CONTINUE

           asize = size(im) & nx = asize(1) & ny = asize(2)

           ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /average

           template = template_create(im, med_spec)
           im = im/template

           ;; Mark likely cosmic rays as NAN
           badim = mark_cr(im)
           badidx = where(badim gt 0, count)
           if count gt 0.01*N_elements(im) then $
             message, 'Too many hot pixels (possibly organized in lines) to make a good measurement'
           if count gt 0 then $
             im[badidx] = !values.f_nan

           if keyword_set(TV) then display, im, /reuse

;        ;; Make a reference image to rotate around 
;        ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /average
;
;        ;; Tried using averages and got about the same answer.  This
;        ;; is probably better, since it gets rid of cosmic ray hits
;        ;; automatically.
;        ref_im=template_create(im, med_xdisp)
;
;        ;; Center of the data image should be close enough to the
;        ;; real center of the spectral image so that rotating the
;        ;; reference image back and forth doesn't create offset
;        ;; problems, which would skew the results.
;        for ri=0,nsteps-1 do begin
;           angle=stepsize*(ri-nsteps/2.)
;           rot_im=rot(ref_im, angle, /PIVOT,  $
;                      1., nx/2., sli_cents[i], $
;                      cubic=-0.5, missing = !values.f_nan)
;           correlations[ri] = total(im*rot_im,/NAN)
;        endfor
;        xaxis = stepsize*indgen(nsteps) - stepsize*nsteps/2.
;        fit = mpfitpeak(xaxis, correlations, params, nterms=nterms)
;
;        if keyword_set(showplots) then begin
;           wset,6
;           plot, xaxis, correlations, psym=asterisk, title=files[i], $
;                 xtitle='Angle of reference image (degrees)', $
;                 ytitle='Correlation coefficient'
;           oplot, xaxis, fit, linestyle=solid
;        endif
;        if abs(params[1]) gt stepsize*nsteps/2. then $
;          message, 'WARNING: measured rotation angle of ' + string(params[1]) + ' outside the region of angles tried.  Assuming file is OK, consider increasing nsteps and/or stepsize'
;        m_cam_rots[i] = params[1]


           asize=size(im) & nx=asize[1] & ny=asize[2]
           ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /average

           ;; Tried using averages and got about the same answer.  This
           ;; is probably better, since it gets rid of cosmic ray hits
           ;; automatically.
           ref_im=template_create(im, med_xdisp)

           ;; Here is a trick to speed automation.  The measured slicer
           ;; center should be the best one to do this rotation about.
           ;; I want to fit the slicer centers as a function of time to
           ;; see how flatfields, etc. should be aligned or thrown out
           ;; Just in case something wasn't assigned
           sli_cent = m_sli_cents[i]
           if NOT finite(sli_cent) then $
             sli_cent = sli_cents[i]
           if sli_cent eq 0 or NOT finite(sli_cent) then $
             sli_cent = ny/2.

           ;; Incidently, this code could also be used to fit the slicer center
           message, 'Hit the S key to skip this fit.  Depress and hold the D key to display images of each fitting iteration.',/CONTINUE
           to_pass = { image:im, ref_im:ref_im, sli_cent:sli_cent }
           M_cam_rots[i] = tnmin('camrot_compare', 0., $
                                 FUNCTARGS=to_pass, /AUTODERIVATIVE, $
                                 /MAXIMIZE, MAXITER=MAXITER)
           

           ngood = ngood + 1
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
     if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  endif ;; not reviewing

  if NOT keyword_set(noninteractive) then begin
     marked_ndays = ssg_mark_bad(ndays, m_cam_rots, $
                                 title=string('Camera rotation in ', indir), $
                                 xtickunits='Hours', $
                                 xtitle=string('UT time (Hours) ', utdate), $
                                 ytitle='Angle (degrees clockwise)', $
                                 window=7)

     dbclose
     
     bad_idx = where(finite(marked_ndays) eq 0, count)
     ;; Beware the cumulative effect here
     if count gt 0 then badarray[bad_idx] = badarray[bad_idx] + 2048

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

  endif ;; interactive

  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'bad, m_cam_rot', badarray, m_cam_rots
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated camera rotation values in ' + dbname
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end
