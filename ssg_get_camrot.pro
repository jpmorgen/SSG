;+
; $Id: ssg_get_camrot.pro,v 1.2 2002/11/12 21:01:31 jpmorgen Exp $

; ssg_get_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_get_camrot, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, pos=pos, nsteps=nsteps, stepsize=stepsize, contrast=contrast

;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(nsteps) then nsteps=11 ; 10 steps centered on 0 degrees
  if NOT keyword_set(stepsize) then stepsize=1. ; 1 degree steps in angle
  if NOT keyword_set(order) then order=2
  if NOT keyword_set(contrast) then contrast = 0.5

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

  dbext, entries, "fname, nday, date, typecode, bad", $
         files, ndays, dates, typecodes, badarray
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(ndays)     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)
  correlations = fltarr(nsteps)
  angles = fltarr(nf) 
  angles[*] = !values.f_nan

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
        if badarray[i] gt 16383 then message, 'Bad overclock region'
        if typecodes[i] lt 4 then message, 'Can''t get a good camera rotation measurement from bias, dark, or comp images'

        im = ssgread(files[i], hdr, /DATA)
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'WARNING: works better if you call ssg_biassub first', /CONTINUE


        ;; Divide out the spectrum to bring out the slicer features
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

        ;; Look at the cross-dispersion spectrum to see if it has
        ;; enough contrast to do us any good
        ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /average

;;        if min(med_xdisp, /NAN) gt contrast*max(med_xdisp, /NAN)
;;        then $
;;        print, median(med_xdisp), mean(med_xdisp, /NAN), stddev(med_xdisp, /NAN)
;;        if stddev(med_xdisp, /NAN) lt contrast*median(med_xdisp) then $
;;          message, 'Not enough contrast in cross-dispersion spectrum to pull out camera rotation'

        ;; Tried using averages and got about the same answer.  This
        ;; is probably better, since it gets rid of cosmic ray hits
        ;; automatically.
        ref_im=template_create(im, med_xdisp)

        ;; Center of the data image should be close enough to the
        ;; real center of the spectral image so that rotating the
        ;; reference image back and forth doesn't create offset
        ;; problems, which would skew the results.
        for ri=0,nsteps-1 do begin
           angle=stepsize*(ri-nsteps/2.)
           rot_im=rot(ref_im, angle, cubic=-0.5) 
           correlations[ri] = total(im*rot_im,/NAN)
        endfor
        xaxis = stepsize*indgen(nsteps) - stepsize*nsteps/2.
        fit = gaussfit(xaxis, correlations, params)

        if keyword_set(showplots) then begin
           wset,6
           plot, xaxis, correlations, psym=asterisk, title=files[i]
           oplot, xaxis, fit, linestyle=solid
        endif
        if abs(params[1]) gt stepsize*nsteps/2. then $
          message, 'WARNING: measured rotation angle of ' + params[1] + ' outside the region of angles tried.  Assuming file is OK, consider increasing nsteps and/or stepsize'
        angles[i] = params[1]
        ngood = ngood + 1
     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  dbclose

  if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  window,6
  plot, ndays, angles, $
        title=string('Camera rotation angles in ', indir), $
        xtickunits='Hours', $
        yrange=[min([angles,angles], /NAN), $
                max([angles, angles], /NAN)], $
        xstyle=2, ystyle=2, psym=plus, $
        xtitle=string('UT time (Hours) ', utdate), $
        ytitle='Angle (degrees)'

  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'm_cam_rot', angles
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated camera rotation in ' + dbname

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end
