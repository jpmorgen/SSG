;+
; $Id: ssg_get_camrot.pro,v 1.5 2003/06/11 18:16:20 jpmorgen Exp $

; ssg_get_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_get_camrot, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, noninteractive=noninteractive, review=review, write=write, maxiter=maxiter, max_angle=max_angle, start_angle=start_angle, nsteps=nsteps, nterms=nterms, trimspec=trimspec

;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(maxiter) then maxiter=10  
  if NOT keyword_set(start_angle) then start_angle=0.
  if NOT keyword_set(nsteps) then nsteps=25
  if NOT keyword_set(trimspec) then trimspec=16
  params = [0, start_angle]

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

  dbext, entries, 'fname, nday, date, typecode, bad', $
         files, ndays, dates, typecodes, badarray
  dbext, entries, 'm_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, m_cam_rot, e_cam_rot', $
         m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, m_cam_rots, e_cam_rots
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)
  if NOT keyword_set(review) then begin ; We really want to do all the fitting

     m_cam_rots[*] = !values.f_nan

     if keyword_set(showplots) then begin
        window,6
        window,7
     endif

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
           if typecodes[i] lt 2 then message, 'Can''t get a camera rotation measurement from bias or dark images'

           im = ssgread(files[i], hdr, eim, ehdr, /DATA, /TRIM)
           test = strtrim(sxpar(hdr,'SLI_CENT',COUNT=count))
           if count eq 0 then message, 'ERROR: you must call ssg_[get&fit]_sliloc first'

           if NOT keyword_set(max_angle) then begin
              max_angle = 0.25
              ;; Binned measurments can use smaller angles 
              ccdsum = strtrim(sxpar(hdr,'CCDSUM',COUNT=count),2)
              if count gt 0 then begin
                 if ccdsum eq '1 4' then begin
                    max_angle = max_angle/4.
                 endif
              endif
           endif

           asize = size(im) & nx = asize(1) & ny = asize(2)

           ;; Trim off the ends of the spectrum to prevent any weird
           ;; CCD edge effects from bothering us
           if trimspec gt 0 then begin
              im[0:trimspec-1,*] 	= !values.f_nan
              im[nx-trimspec-1:nx-1,*] 	= !values.f_nan
           endif
           ;; Borrow some code from ssg_lightsub to get rid of any
           ;; background light problem (particularly troublesome in
           ;; comps).  ssg_lightsub works better with the rotation
           ;; taken out, which is why we don't do it for real before
           ;; now.
           sli_bots = ceil(sli_bots)
           sli_tops = floor(sli_tops)
           edge_im = fltarr(nx,ny-(sli_tops[i]-sli_bots[i]))
           edge_im[*,0:sli_bots[i]-1] = im[*,0:sli_bots[i]-1]
           edge_im[*,sli_bots[i]:ny-(sli_tops[i]-sli_bots[i])-1] = $
             im[*,sli_tops[i]:ny-1]
           edge_spec = fltarr(nx)
           for ix=0,nx-1 do begin
              ;; Calculate median only, since there is certain to be
              ;; contamination from the wing of the light coming through
              ;; the slicer/exit slit jaws that would mess up the average
              edge_spec[ix] = median(edge_im[ix,*])
           endfor
           ;;plot, edge_spec
           template = template_create(im, edge_spec)
           im = im - template

           if keyword_set(showplots) then wset,7
           ssg_spec_extract, im[*,sli_bots[i]:sli_tops[i]], $
                             hdr, spec, xdisp, showplots=showplots, $
                             med_spec=med_spec, med_xdisp=med_xdisp, /average

           ;; For everything but comps, remove cosmic rays.  CR
           ;; removal from comps doesn't work very well because of
           ;; high contrast in good signal
           if typecodes[i] gt 2 then begin
              template = template_create(im, normalize(med_spec))
              im = im/template
              ;; Mark likely cosmic rays as NAN
              badim = mark_cr(im)
              badidx = where(badim gt 0, count)
              if count gt 0.01*N_elements(im) then $
                message, 'Too many hot pixels (possibly organized in lines) to make a good measurement'
              if count gt 0 then $
                im[badidx] = !values.f_nan
           endif ;; not a comp

           if keyword_set(TV) then display, im, /reuse

           ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /average

           if typecodes[i] gt 2 then begin
              ;; For non-comp images, this removes additional cosmic
              ;; ray effects
              ref_im=template_create(im, med_xdisp)
           endif else begin
              ;; For the comps, the median xdisp spectrum is 0, so use
              ;; the average instead
              ref_im=template_create(im, xdisp)
           endelse


           ;; Here is a trick to speed automation.  The measured slicer
           ;; center should be the best one to do this rotation about.
           ;; I want to fit the slicer centers as a function of time to
           ;; see how flatfields, etc. should be aligned or thrown out
           ;; Just in case something wasn't assigned
           sli_cent = sli_cents[i]
           if finite(sli_cent) eq 0 then $
             sli_cent = sli_cents[i]
           if sli_cent eq 0 or finite(sli_cent) eq 0 then $
             sli_cent = ny/2.

           num_tries = 0
           repeat begin
              center = params[1]
;           ;; Incidently, this code could also be used to fit the slicer center
;           message, 'Hit the S key to skip this fit.  Depress and hold the D key to display images of each fitting iteration.',/CONTINUE
;           to_pass = { image:im, ref_im:ref_im, sli_cent:sli_cent }
;           max_center = tnmin('camrot_compare', 0., $
;                              FUNCTARGS=to_pass, /AUTODERIVATIVE, $
;                              /MAXIMIZE, MAXITER=MAXITER, $
;                              FGUESS=total(im^2, /NAN))
;
;           print, max_center

              stepsize = max_angle*2./nsteps
              correlations = fltarr(nsteps)

              ;; Center of the data image should be close enough to
              ;; the real center of the spectral image so that
              ;; rotating the reference image back and forth doesn't
              ;; create offset problems, which would skew the results.
              angles = stepsize*(indgen(nsteps) - nsteps/2.) + center

              for ri=0,nsteps-1 do begin
                 rot_im=ssg_camrot(ref_im, angles[ri], nx/2., sli_cent)
                 correlations[ri] = total(im*rot_im,/NAN)
              endfor
              fit = mpfitpeak(angles, correlations, params, nterms=nterms, $
                              error=sqrt(correlations), perror=perror, $
                              /POSITIVE)
           
              if keyword_set(showplots) then begin
                 wset,6
                 plot, angles, correlations, psym=asterisk, title=files[i], $
                       xtitle='Angle of reference image (degrees)', $
                       ytitle='Correlation coefficient', $
                       yrange=[min(correlations), max(correlations)], ystyle=2
                 oplot, angles, fit, linestyle=solid
                 oploterr, angles, correlations, sqrt(correlations)
              endif
              num_tries = num_tries + 1
           endrep until abs(params[1] - center) lt stepsize*nsteps/3. $
             or num_tries eq 3
           m_cam_rots[i] = params[1]
           e_cam_rots[i] = perror[1]
           print, params[1], perror[1]
           ngood = ngood + 1
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
     if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  endif ;; not reviewing

  if NOT keyword_set(noninteractive) then begin
     marked_ndays = ssg_mark_bad(ndays, m_cam_rots, $
                                 measure_errors=e_cam_rots, $
                                 title=string('Camera rotation in ', indir), $
                                 xtickunits='Hours', $
                                 xtitle=string('UT time (Hours) ', utdate), $
                                 ytitle='Angle (degrees clockwise)', $
                                 window=7, /MJD)

     dbclose
     
     bad_idx = where(finite(marked_ndays) eq 0, count)

     if count gt 0 then badarray[bad_idx] = badarray[bad_idx] OR 2048

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
     dbupdate, entries, 'bad, m_cam_rot, e_cam_rot', $
               badarray, m_cam_rots, e_cam_rots
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated camera rotation values in ' + dbname
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end
