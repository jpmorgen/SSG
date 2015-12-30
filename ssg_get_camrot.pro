;+
; $Id: ssg_get_camrot.pro,v 1.7 2015/03/04 15:52:58 jpmorgen Exp $

; ssg_get_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

function rot_compare, angle ;;, dp, im=im, ref_im=ref_im, sli_cent=sli_cent, nx=nx
  ;; tnmin doesn't need common blocks but is slow for this application
  COMMON ssg_get_camrot, im, ref_im, sli_cent, nx, hdr, typecode, orig_med_spec, orig_norm_xdisp, column_mask
  if keyword_set(dp) then $
     message, 'ERROR: You are asking me to calculate a derivative of the parameters for tnmin.  I don''nt know how to do this.  Make sure you specify /AUTODERIVATIVE with tnmin'

  ;; I am not sure why amoeba passes two arguments when I just want
  ;; one, but that was an easy enough fix.
  ;;rot_im = ssg_camrot(ref_im, angle[0], nx/2., sli_cent, /quiet)
  ;;return, -total(im*rot_im,/NAN)

  ;; Tue Dec 22 10:19:08 2015  jpmorgen@snipe
  
  ;; Trying for a better algorithm.  The idea is to find the angle
  ;; that produces a 2D spectrum that collapses and reexpands well
  ;; into a template which matches the original spectrum the
  ;; best.  Didn't have any measureable effect
  
  rot_im = ssg_camrot(im, -angle[0], nx/2., sli_cent, /quiet)
  ;; Make sure whole first and last columns are set to NAN to keep
  ;; number of pixels 
  rot_im[0,*] = !values.f_NAN
  rot_im[nx-1,*] = !values.f_NAN
  rot_im *= column_mask

  ssg_spec_extract, rot_im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /average
  if typecode eq 2 then begin
     ;; For the comps, the median xdisp spectrum is 0, so use
     ;; the average instead and just try to line things up based on
     ;; the edges of the spectrum
     ref_im=template_create(rot_im, xdisp)
     ;; And just return our basic macro rotation figure of merit 
     return, -total((rot_im*ref_im)^2.,/NAN)
  endif
  ;; If we made it here, we are working with flats and object images.

  ;; Try to work with the details of the slicer pattern.  I seem to
  ;; get better results, though still not perfect, if I create the
  ;; template with the use cross-dispersion only and divide, rather
  ;; than do a traditional residual

  ;;ref_im=template_create(rot_im, orig_med_spec, med_xdisp)
  ref_im=template_create(rot_im, med_xdisp)
  ;;FOM = total((rot_im - ref_im)^2, /NAN)

  ;; Cut off edges
  ;; This has to be a fairly low value (<0.5) in order to create
  ;; contrast for the algoritm to see edges, particularly the flats
  ;; Trying with with /data/io/ssg/reduced/1994/940714wide/, which has
  ;; a large angle to get away from the orig_norm_xdisp, which seems
  ;; to have biased the results
  ;;bad_idx = where(orig_norm_xdisp lt 0.1, count)
  bad_idx = where(normalize(rot_im, 0.75) lt 0.1, count)
  if count gt 0 then $
     rot_im[bad_idx] = !values.f_NAN

  ;; Flatten by the ref_im
  rot_im /= ref_im

  FOM = total(rot_im^2, /NAN)

  ;;atv, rot_im
  ;;print, angle[0], FOM
  return, FOM

  ;;;;;; Try to make a reasonable penalty for being off large amounts
  ;;;;if count gt 0 then begin
  ;;;;   med_xdisp[bad_idx] = 1
  ;;;;endif
  ;;   
  ;;
  ;;;; Figure of merit for macro rotation.  Goes up when aligned
  ;;;; Since we don't have flats yet for determining precise edges, work
  ;;;; with normalize to make sure we don't divide by things that are
  ;;;; too small
  ;;;;ref_im = normalize(ref_im, 0.75, /mean)
  ;;macro_FOM = total((rot_im*ref_im)^2.,/NAN)
  ;;
  ;;;; Now create a figure of merit that decreases when the lumps from
  ;;;; misalignment diminish
  ;;;;rot_im = normalize(rot_im^2)
  ;;slicer_FOM = total(rot_im^2, /NAN)
  ;;print, angle[0], macro_FOM, slicer_FOM, slicer_FOM-macro_FOM
  ;;return, slicer_FOM;;-macro_FOM
  ;;;;return, total(rot_im-ref_im,/NAN)

end

pro ssg_get_camrot, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, noninteractive=noninteractive, review=review, write=write, ftol=ftol, maxiter=maxiter, max_angle=max_angle, start_angle=start_angle, nsteps=nsteps, nterms=nterms, trimspec=trimspec

  COMMON ssg_get_camrot, im, ref_im, sli_cent, nx, hdr, typecode, orig_med_spec, orig_norm_xdisp, column_mask
;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(ftol) then ftol = 1E-5
  if NOT keyword_set(start_angle) then start_angle=0.
  if NOT keyword_set(nsteps) then nsteps=25
  if NOT keyword_set(trimspec) then trimspec=0
  if NOT keyword_set(nterms) then nterms=5
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
  entries = dbfind(string("dir=", indir), /fullstring)

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
  this_nday = median(fix(ndays)) ; presumably this will throw out anything taken at an odd time
  
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
           CONTINUE
        endif ;; catching error

        ;; Skip bad images
        if badarray[i] ge 4096 then $
           CONTINUE
        ;; Can't get a camera rotation measurement from bias or dark images
        if typecodes[i] lt 2 then $
           CONTINUE

        im = ssgread(files[i], hdr, eim, ehdr, /DATA, /TRIM)
        test = strtrim(sxpar(hdr,'SLI_CENT',COUNT=count))
        if count eq 0 then message, 'ERROR: you must call ssg_[get&fit]_sliloc first'

        typecode = typecodes[i]

        ;; Set step size for amoeba
        if NOT keyword_set(max_angle) then begin
           ;; Estimate 5 pixels across whole CCD as max we could be off
           max_angle=!radeg*atan(5/800.)
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

        ;; Replace these and any other bad columns with extensions
        ;; from their edge values so that NAN areas don't affect the
        ;; calculations
        im = ssg_column_replace(im, column_mask, grow_mask=3)

        ;; Borrow some code from ssg_lightsub to get rid of any
        ;; background light problem (particularly troublesome in
        ;; comps).  ssg_lightsub works better with the rotation
        ;; taken out, which is why we don't do it for real before
        ;; now.
        edge_mask = fltarr(nx,ny)+1.
        edge_mask[*,sli_bots[i]:sli_tops[i]] = !values.f_nan
        edge_im = im * edge_mask
        edge_spec = fltarr(nx)
        for ix=0,nx-1 do begin
           ;; Calculate median only, since there is certain to be
           ;; contamination from the wing of the light coming through
           ;; the slicer/exit slit jaws that would mess up the average
           edge_spec[ix] = median(edge_im[ix,*])
        endfor ;; ix
        ;;plot, edge_spec
        good_idx = where(finite(edge_spec) eq 1, count)
        if count gt 0 then begin
           template = template_create(im, edge_spec)
           im = im - template
        endif

        if keyword_set(showplots) then wset,7

        
        ;; For stable performance in rot_compare, use the original
        ;; dispersion spectrum within the good region of the spectrum
        ;; for creating the template and the full cross dispersion
        ;; spectrum for defining bad rows
        ssg_spec_extract, im[*,sli_bots[i]:sli_tops[i]], $
                          hdr, showplots=showplots, $
                          med_spec=orig_med_spec, /average
        ssg_spec_extract, im, hdr, med_xdisp=orig_xdisp, /average
        orig_norm_xdisp = normalize(orig_xdisp, 0.75, /mean)
        ;; Get ready to replace the edges with original pixels
        edge_idx = where(orig_norm_xdisp lt 0.1, edge_count, complement=middle_idx)
        ;;edge_idx = [indgen(sli_bots[i]), sli_tops[i] + indgen(sli_tops[i])]

        ;; For everything but comps, remove cosmic rays.  CR
        ;; removal from comps doesn't work very well because of
        ;; high contrast in good signal
        if typecodes[i] gt 2 then begin
           ;; Do our cosmic ray removal separately for edges and
           ;; middle.  The edges need to be flattened by the
           ;; cross dispersion spectrum, since there is a shape going
           ;; down from the cut we used to define edge_idx.  We also
           ;; need to be careful of divide by zero, so bump up by the
           ;; minimum value
           oim = im
           edge_im = im
           edge_im[*, middle_idx] = !values.f_NAN
           min_edge = min(edge_im, /NAN)
           edge_im -= min_edge
           template = template_create(edge_im, orig_xdisp-min_edge)
           edge_im = edge_im/template
           bad_edge_im = mark_bad_pix(edge_im)
           ;; Now do the middle
           cim = im
           template = template_create(im, orig_med_spec, orig_xdisp)
           cim = cim/template
           if edge_count gt 0 then $
              cim[*,edge_idx] = !values.f_NAN
           badim = mark_bad_pix(cim)
           badidx = where(bad_edge_im+badim gt 0, count)
           if count gt 0.01*N_elements(cim) then $
              message, 'Too many hot pixels (possibly organized in lines) to make a good measurement'
           ;; Take out cosmic rays from original image
           if count gt 0 then $
              im[badidx] = !values.f_nan
        endif ;; not a comp

        if keyword_set(TV) then display, im, /reuse, zoom=2


        ;;ssg_spec_extract, im, hdr, spec, xdisp, med_spec=med_spec, med_xdisp=med_xdisp, /average
        ;;
        ;;if typecodes[i] gt 2 then begin
        ;;   ;; For non-comp images, this removes additional cosmic
        ;;   ;; ray effects
        ;;   ref_im=template_create(im, med_xdisp)
        ;;endif else begin
        ;;   ;; For the comps, the median xdisp spectrum is 0, so use
        ;;   ;; the average instead
        ;;   ref_im=template_create(im, xdisp)
        ;;endelse


        ;; Here is a trick to speed automation.  The measured slicer
        ;; center should be the best one to do this rotation about.
        ;; I want to fit the slicer centers as a function of time to
        ;; see how flatfields, etc. should be aligned or thrown out
        ;; Just in case something wasn't assigned
        sli_cent = sli_cents[i]
        sli_bot = sli_bots[i]
        sli_top = sli_tops[i]
        if finite(sli_cent) eq 0 then $
           sli_cent = sli_cents[i]
        if sli_cent eq 0 or finite(sli_cent) eq 0 then $
           sli_cent = ny/2.

        ;;to_pass = { im:im, ref_im:ref_im, sli_cent:sli_cent, nx:nx }
        ;; tnmin was pretty slow
        ;;cam_rot = tnmin('rot_compare', 0.,  $
        ;;               FUNCTARGS=to_pass, /AUTODERIVATIVE, $
        ;;               /MAXIMIZE)
        cam_rot = amoeba(ftol, function_name='rot_compare', $
                         function_value=function_value, $
                         p0=start_angle, scale=max_angle)
        if cam_rot eq -1 then $
           message, 'ERROR: can''t get camrot on this one'
        m_cam_rots[i] = cam_rot
        e_cam_rots[i] = ftol
        ngood = ngood + 1
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
