;+

; $Id: ssg_fit1spec.pro,v 1.6 2004/02/09 19:27:03 jpmorgen Exp $

; ssg_fit1spec.pro

; We want to build a database of atmospheric, solar, and Io emission
; lines that will be fit to the SSG spectrum.  At this high resolution
; and with boarderline signal-to-noise, it is useful to have these
; lines devided up into various groups, e.g. strong solar, stong
; atmospheric, weak solar, etc.  As time goes on, the database of
; useful lines will grow + some weird shapes might evolve which are
; non-voigt like (e.g. saturated lines).  I will try to build in
; compatability for these things, but for now, let's start with
; Voigts.

;-

function modval, val, name, format=format
  newval=val
  if NOT keyword_set(format) then $
    format = '(a, " [", f10.4, "] ")'
  prompt = string(format=format, name, newval)
  stringval=''
  read, prompt=prompt, stringval
  if strlen(stringval) gt 0 then $
    newval = double(stringval)
  return, newval
end

function modstring, val, name, format=format
  newval=val
  if NOT keyword_set(format) then $
    format = '(a, " [", a, "] ")'
  prompt = string(format=format, name, newval)
  stringval=''
  read, prompt=prompt, stringval
  if strlen(stringval) gt 0 then $
    newval=stringval
  return, newval
end

;; Seems to need to have the whole structure, otherwise copies are
;; made + are hard to return
pro modpar, idx_list, params, parinfo
  npar = N_elements(idx_list)
  if npar eq 0 then return
  if npar eq 1 then idx_list=[idx_list]
  for i = 0,npar-1 do begin
     idx = idx_list[i]
     message, 'Modifing parameter named "' + parinfo[idx].parname + '".  Let''s start with the name itself, followed by its value and then the various constraint information that MPFITFUN uses.', /CONTINUE

     parinfo[idx].parname = modstring(parinfo[idx].parname, 'Parameter name')
     params[idx] = modval(params[idx], parinfo[idx].parname)
     parinfo[idx].fixed = fix(modval(parinfo[idx].fixed, 'Fixed?', $
                                     format='(a, " [", i1, "] ")'))
     parinfo[idx].limited[0] = fix(modval(parinfo[idx].limited[0], $
                                          'Limited on left side?', $
                                          format='(a, " [", i1, "] ")'))
     parinfo[idx].limited[1] = fix(modval(parinfo[idx].limited[1], $
                                          'Limited on right side?', $
                                          format='(a, " [", i1, "] ")'))
     parinfo[idx].limits[0] = modval(parinfo[idx].limits[0], $
                                     'Left limit')
     parinfo[idx].limits[1] = modval(parinfo[idx].limits[1], $
                                     'Right limit')
;      parinfo[idx].step = modval(parinfo[idx].step, $
;                                 '".step" Step size for numerical derivatives')
;      parinfo[idx].mpside = fix(modval(parinfo[idx].mpside, $
;                                       '".mpside" Sidedness of the finite difference', $
;                                format='(a, " [", i2, "] ")'))
;      
;      parinfo[idx].mpmaxstep = modval(parinfo[idx].mpside, $
;                                     '".mpmaxstep" Max change in parameter value per iteration')
     parinfo[idx].tied = modstring(parinfo[idx].tied, $
                                   'Tied expression (needs absolute parameter reference, e.g. 2 * P(1)')
;     parinfo[idx].mpprint = fix(modval(parinfo[idx].mpprint, $
;                                       'Print this parameter in iterations?', $
;                                       format='(a, " [", i1, "] ")'))

  endfor
  message, 'Thank you!', /CONTINUE
  for ki = 0,1000 do flush_input = get_kbrd(0)
end

pro ssg_fit1spec, nday, N_continuum=N_continuum_orig, maxiter=maxiter, quiet=quiet, $
                  nprint=nprint

  init = {ssg_sysvar}
  init = {tok_sysvar}

  if N_elements(nprint) eq 0 then nprint=10

  if N_elements(maxiter) eq 0 then maxiter = 50

  if N_elements(nday) ne 1 then $
    message, 'ERROR: supply one and only nday at a time'
  if nday eq -1 then begin
     message, 'NOTE: encountered nday = -1 flag, terminating', /INFORMATIONAL
     return
  endif

  if N_elements(N_continuum_orig) eq 0 then N_continuum_orig = 1

  rdbname = 'ssg_reduce'
  fdbname = 'oi_6300_fit'

  message, /INFO, 'fitting spectrum nday = ' + string(nday)

  ;; Check on the source of these and add to it.  These come from
  ;; Melanie.
  weak_solar_lines = [6286.3d, $
                      6286.64d, $
                      6298.97d, $
                      6301.0d, $
                      6301.86d]

  ;; Doing a residual analysis on 10/14/03.  Weak is no longer the
  ;; right word for the feature to the blue of 6299.5957.  Since I am
  ;; treating this separately, I might as well do the whole line
  ;; complex.  Try putting a line the same distance away from the
  ;; 6301.5115 line
  broad_solar_lines = [6299.29d, 6299.5957d, 6301d]
  broad_solar_line_EWs = [-5, -40, -5]

  ;; This is the atmospheric line list that Melanie was using.  
;  atm_absorb = [6298.457d, 6299.228d, 6302.0001d, 6302.7642d];

; I am not sure where this one came from: , 6304.53d

  ;; I got the weak atm absorption lines by fitting residuals.
  ;; Sometimes these are not so weak!
  weak_atm_lines = [6290.85d, 6292.575d, 6294.675d]
  
  atm_emi = [6300.304d]
  io_lines = [6300.304d]     ; I want to eventually put the Na in here

  dbopen, rdbname, 0
  rentry = where_nday_eq(nday, COUNT=count,SILENT=silent, tolerance=0.001)
  if count eq 0 then message, $
    'ERROR: nday ' + string(nday) + ' not found in ' + rdbname

  dbext, rentry, 'nday, dir, fname, object, date, time, bad', ndays, dirs, files, objects, dates, times, badarray

  dbext, rentry, 'wavelen, spectrum, spec_err, cross_disp, cross_err, disp_pix, dispers', wavelengths, spectra, spec_errors, cross_disps, cross_errors, disp_pix, orig_dispers

  dbclose
  
  dirs = strtrim(dirs)
  files=strtrim(files)
  objects=strtrim(objects)
  shortfile= strmid(files[0], $
                    strpos(files[0], '/', /REVERSE_SEARCH) + 1)

  dbopen,'io6300_integrated',0  
  io6300_entry = where_nday_eq(nday, $
                               COUNT=count,SILENT=silent, tolerance=0.002) 
  eph_dops = ptr_new()
  if count eq 0 then begin
     message, /continue, 'WARNING: unable to find database entry.  Doppler shifts will start at 0.'
  endif else begin              ; found entry in Melanie's database
     ;; --> NEW STUFF
     ;; Here is where I need to think about how to structure what I
     ;; hope to be getting from the ephemerides.  I think I want to
     ;; put them in a dg_stuct and put that in !sso.dgs.  The easiest
     ;; way to do this is to make some Doppler shift parinfo records
     dbext, io6300_entry, 'deldot, rdot', deldots, rdots
     deldot_par = pfo_fcreate(!pfo.sso_funct, ptype=!sso.dop, $
                              path=[!eph.io, !eph.earth], $
                              value=deldots[0], $
                              parinfo_template=!ssg.parinfo)
     rdot_par   = pfo_fcreate(!pfo.sso_funct, ptype=!sso.dop, $
                              path=[!eph.sun, !eph.io, !eph.earth], $
                              value=rdots[0] + deldots[0], $
                              parinfo_template=!ssg.parinfo)
  endelse                       ; found entry in Melanie's database
  dbclose  

  cd, dirs[0]
  im=ssgread(files[0], hdr, /DATA)
  asize = size(im) & nx = asize(1) & ny = asize(2)

  ;; --> TEMPORARY CODE.  I am going to nuke this after I switch over
  ;; to using the dispersion in the database.
  ;; Read in dispersion coefficients so I can use that as part of the
  ;; fit.  This code is replaced below by reading the database.
  disp_order = 0
  orig_dispers = sxpar(hdr, $
                       string(format='("DISPERS", i1)', disp_order), count=count)
  while count ne 0 do begin
     disp_order = disp_order + 1
    temp = sxpar(hdr, $
                  string(format='("DISPERS", i1)', disp_order), count=count)
     if count ne 0 then $
       orig_dispers = [orig_dispers, temp]
  endwhile
  orig_dispers = [orig_dispers, !values.d_nan]
  disp_order = disp_order - 1

  if disp_order eq 0 then $
    message, 'ERROR: not enough DISPERS keyword found in header of ' + shortfile

  window,6
  title=string(objects[0], ' ', shortfile, ' ', nday2date(ndays[0]), ' (UT)')
  ;; This has to be in font !3 for the angstrom symbol to be found
  xtitle='Wavelength ('+string("305B)+')' ;" ;
  ytitle=string('Signal (', sxpar(hdr, 'BUNIT'), '/S)')
  
  good_pix = where(finite(disp_pix[*,0]) eq 1 and $
                   finite(wavelengths[*,0]) eq 1 and $
                   finite(spectra[*,0]) eq 1 and $
                   finite(spec_errors[*,0]) eq 1, n_pix)
  if n_pix eq 0 then begin
     message, /continue, 'ERROR: no good data found in ' + files[0]
     return
  endif

  left_wval = min(wavelengths, a)
  left_idx = a[0]
  right_wval = max(wavelengths, a)
  right_idx = a[0]

  ref_pixel = nx/2.

  done = 0
  did_fit = 0
  saved = 0
  repeat begin
     
     ;; Extract useful things from parameter list, recording any
     ;; changes since the last iteration.  Also check to see if this
     ;; is the first time around or if we have deleted parameters
     ;; in order for them to be reinitialized.
     
     ;; DISPERSION
     if N_elements(parinfo) eq 0 then begin
        ;; First time through
        disp_order = 0
     endif else begin
        ;; Maybe not the best way to pick dispersion out, but works
        ;; for now
        disp_idx = where(parinfo.pfo.inaxis eq !pfo.Xin and $
                         parinfo.pfo.outaxis eq !pfo.Xaxis, $
                         disp_order)
    endelse
     ;; Check for initilization of params/parinfo
     if disp_order le 1 then begin
        ;; We need an initization or reset.  Re-read the dispersion
        ;; fit by ssg_fit_dispers and change 1st and 2nd order coefs
        ;; to be in ma and microA, respectively
        disp_order = 0
        dispers = double(orig_dispers[disp_order,0])
        while disp_order lt N_elements(orig_dispers)-2 and $
          finite(orig_dispers[disp_order+1,0]) eq 1 do begin
           disp_order = disp_order + 1
           if disp_order le N_elements(orig_dispers)-1 then begin
              dispers = [dispers, double(orig_dispers[disp_order,0])] 
              ;; add scaling back eventually? *10^(3.*disp_order)]
           endif
        endwhile

        ;; With a segmented polynomial, it is a good idea
        disp_par = pfo_fcreate(!pfo.poly, poly_order=disp_order, $
                               poly_ref=ref_pixel, poly_value=dispers, $
                               inaxis=!pfo.Xin, outaxis=!pfo.Xaxis, $
                               fop=!pfo.repl, $
                               parinfo_template=!ssg.parinfo)

;        for idsp = 0,disp_order do begin
;           p = tparams[idsp] 
;           ;; Kind of bogus limits, but thes will keep the mpfit code happy
;           tparinfo[idsp].limits = [double(p - 10d), $
;                                    double(p + 10d)]
;           tparinfo[idsp].parname=string(format='("Disp Coef ", i3)', idsp)
;           tparinfo[idsp].ssgID=ssgid_disp
;        endfor
        ;; INITIALIZE PARAMETER LIST
        if N_elements(parinfo) le 1 then begin
           message, 'NOTE: initializing parameter list' ,/INFORMATIONAL
           parinfo = [disp_par, deldot_par, rdot_par]
        endif ;; Initilize parameter list
        message, 'NOTE: set dispersion coefficients to those found in FITS header of ' + shortfile ,/INFORMATIONAL
     endif ;; Dispersion initialization
     ;; Keep dispersion handy as a separate variable.  Also the
     ;; dispersion order, though with segmented polynomials, this is
     ;; getting a bit obsolete 
     disp_idx = where(parinfo.pfo.inaxis eq !pfo.Xin and $
                      parinfo.pfo.outaxis eq !pfo.Xaxis)
     temp = where(parinfo[disp_idx].pfo.ftype ge 1.1, disp_order)
     disp_order = disp_order-1

     ;; Calculate new wavelength scale
     junk = pfo_funct(disp_pix, parinfo=parinfo, idx=disp_idx, $
                      xaxis=new_wavelengths)

     ;; Handle the results of any wavelength window repositioning,
     ;; including recalculating the X axis for dispersion changes and
     ;; killing off or resurrecting lines 
     if left_idx gt right_idx then begin
        temp = left_idx & left_idx = right_idx & right_idx = temp
     endif
     left_wval =  new_wavelengths[left_idx]
     right_wval = new_wavelengths[right_idx]
     temp = disp_pix
     temp[0:left_idx] = !values.f_nan
     temp[right_idx:nx-1] = !values.f_nan
     pix_axis = where(finite(temp) eq 1 and $
                      finite(wavelengths) eq 1 and $
                      finite(spectra) eq 1 and $
                      finite(spec_errors) eq 1, n_pix)
     if n_pix eq 0 then begin
        message, /continue, 'WARNING: no good data found in selected range'
     endif else begin
        ;; We have good data in our selected wavelength range
        junk = pfo_funct(pix_axis, parinfo=parinfo, idx=disp_idx, xaxis=xaxis)
        spec = spectra[pix_axis]
        err_spec = spec_errors[pix_axis]

        ;; CONTINUUM.  It is important to do it in this order so the
        ;; median spectrum is taken from the stuff in the window, not the
        ;; whole spectrum

        cont_idx = where(parinfo.sso.ptype eq !sso.cont, N_continuum)
        if N_continuum eq 0 then begin
           message, 'NOTE: resetting continuum to median of displayed spectrum and, if N_continuum specified on the command line, zeroing higher order terms ', /INFORMATIONAL
           m = median(spectra)
           ;; Higher order polynomial coefs should be initialized to 0 
           cont_par = pfo_fcreate(!pfo.sso_funct, ptype=!sso.cont, $
                                  sso_ftype=!pfo.poly, $
                                  poly_order=0, value=median(spectra), $
                                  parinfo_template=!ssg.parinfo)
           parinfo = [parinfo, cont_par]
        endif ;; Continuum initialization
        ;; Keep continuum handy as a separate variable
        cont_idx = where(parinfo.sso.ptype eq !sso.cont, N_continuum)

        ;; Make a continuum spectrum for subtraction in graveyard
        ;; calculations.  Using original dispersion axis pixels so bad
        ;; data doesn't interfere with model calculations.
;        cont_spec = pfo_funct(disp_pix[left_idx:right_idx], parinfo=parinfo, $
;                              idx=[disp_idx, cont_idx])
;
        cont_spec = pfo_funct(disp_pix[left_idx:right_idx], parinfo=parinfo, $
                              idx=[cont_idx])

        ;; DOPPLER SHIFTS
        sso_dg_assign, parinfo

; Save this code for later, if at all.
;        ;; If we have redshifted a line into our field, we need to
;        ;; go back out to the blue to find it in our catalogs, so
;        ;; calculate arrays of left and right wavelength limits for each
;        ;; Doppler group.
;        left_dop_wvals = left_wval  * ( 1 - dopplers / c )
;        right_dop_wvals = right_wval  * ( 1 - dopplers / c )

        ;; Do a preliminary run of the model so we can recalculate the
        ;; observed wavelenghts so the graveyard works properly
        model_spec = pfo_funct(pix_axis, parinfo=parinfo, $
                              idx=[disp_idx, cont_idx])


        ;; GRAVEYARD.  

        ;; Find the line center indexes of things that should potentially
        ;; be removed from the active line list
        grave_lc_idx = where(parinfo.sso.ttype eq !sso.center and $
                             (parinfo.sso.owl lt left_wval or $
                              right_wval lt parinfo.sso.owl), num_to_grave)
        for il=0,num_to_grave - 1 do begin
           ;; find all the parameters for this line
           c_idx = grave_lc_idx[il] ; center idx
           dg = parinfo[c_idx].sso.dg ; Doppler group
           rwl = parinfo[c_idx].sso.rwl ; rest wavelength
           message, 'NOTE: Looking at graveyard candidate ' + strjoin(sso_dg_path(dg, /name), '-') + string(format=!sso.rwl_format, rwl), /INFORMATIONAL

           myidx = where(parinfo.sso.dg eq dg and parinfo.sso.rwl eq rwl)

           ;; Now calculate the model spectrum with just this line in it
           ;; and compare that to a model spectrum with the line in the
           ;; middle of the spectrum to see if we are losing too much area
           model_spec = pfo_funct(disp_pix[left_idx:right_idx], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, myidx])
           model_spec = model_spec - cont_spec
           off_area = total(model_spec, /NAN)
           my_lc_idx = where(parinfo[myidx].sso.ttype eq !sso.center, count)
           ;; Treat multiple lines with the same rest wavelength as one line
;           if count gt 1 or count eq 0 then begin
;              message, 'ERROR: unexpected number of lines with rwl=' + string(rwl)
;              message, 'WARNING: possible multiple entries in catalog ' + string(testparinfo[my_lc_idx[0]].ssggroupID) + ' for rest wavelength ' + string(testparinfo[my_lc_idx[0]].ssgrwl), /CONTINUE
;              testparinfo[my_lc_idx].ssgowl = !values.f_nan          
;           endif

           ;; Unwrap indices
           my_lc_idx = myidx[my_lc_idx]
           ;; Since pfo_funct recalculates owl, we need to tweak rwl
           ;; to put the line in the center of the spectrum.  For
           ;; spectra with significant non-linear dispersion, this
           ;; won't work, but that is not the case for ssg.
           dw = (left_wval + right_wval)/2. - parinfo[my_lc_idx].sso.owl
           saverwl = parinfo[my_lc_idx].sso.rwl
           parinfo[my_lc_idx].sso.rwl = parinfo[my_lc_idx].sso.rwl - dw

           model_spec = pfo_funct(disp_pix[left_idx:right_idx], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, myidx])
           parinfo[my_lc_idx].sso.rwl = savewrl

           model_spec = model_spec - cont_spec
           on_area = total(model_spec, /NAN)
           ;; This ratio is somewhat model and instrument profile
           ;; dependent, but I don't want to make it a command line
           ;; parameter just yet.  Split the difference between 0.5,
           ;; which is the symetric line on the edge case and something
           ;; really severe like 0.01, which would potentially be prone
           ;; to blowing up.  Also, handle the pathological case of 0
           ;; area 
           if off_area - on_area eq 0 or off_area/on_area lt 0.05 then begin
              ;; This line belongs in the graveyard.
              message, 'NOTE: Sending ' + strjoin(sso_dg_path(dg, /name), '-') + string(format=!sso.rwl_format, rwl), /INFORMATIONAL
              parinfo[myidx].pfo.status = !pfo.inactive
           endif ;; Moved a line off to the graveyard
        endfor ;; Moving lines to the graveyard

        ;; Find the line center indexes of things that should potentially
        ;; be resurected from the graveyard
        resur_lc_idx = where(parinfo.sso.ttype eq !sso.center and $
                             (left_wval lt parinfo.sso.owl and $
                              parinfo.sso.owl lt right_wval), num_resur)

        for il=0,num_resur - 1 do begin
           ;; find all the parameters for this line
           c_idx = resur_lc_idx[il] ; center idx
           dg = parinfo[c_idx].sso.dg ; Doppler group
           rwl = parinfo[c_idx].sso.rwl ; rest wavelength

           message, 'NOTE: Looking at resurrection candidate ' + strjoin(sso_dg_path(dg, /name), '-') + string(format=!sso.rwl_format, rwl), /INFORMATIONAL

           myidx = where(parinfo.sso.dg eq dg and parinfo.sso.rwl eq rwl)

           ;; Now calculate the model spectrum with just this line in it
           ;; and compare that to a model spectrum with the line in the
           ;; middle of the spectrum to see if we have gained back enough
           ;; area to add the line
           model_spec = pfo_funct(disp_pix[left_idx:right_idx], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, myidx])
           model_spec = model_spec - cont_spec
           off_area = total(model_spec, /NAN)
           my_lc_idx = where(parinfo[myidx].sso.ttype eq !sso.center, count)
           ;; Treat multiple lines with the same rest wavelength as one line

           ;; Unwrap indices
           my_lc_idx = myidx[my_lc_idx]
           ;; Since pfo_funct recalculates owl, we need to tweak rwl
           ;; to put the line in the center of the spectrum.  For
           ;; spectra with significant non-linear dispersion, this
           ;; won't work, but that is not the case for ssg.
           dw = (left_wval + right_wval)/2. - parinfo[my_lc_idx].sso.owl
           saverwl = parinfo[my_lc_idx].sso.rwl
           parinfo[my_lc_idx].sso.rwl = parinfo[my_lc_idx].sso.rwl - dw

           model_spec = pfo_funct(disp_pix[left_idx:right_idx], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, myidx])
           parinfo[my_lc_idx].sso.rwl = savewrl

           model_spec = model_spec - cont_spec
           on_area = total(model_spec, /NAN)

           ;; SEE DOCUMENTATION ABOVE.  But it is a little harder to do
           ;; on the way out, since I use the window edge as the trigger
           if off_area - on_area ne 0 and off_area/on_area ge 0.5 then begin
              message, 'NOTE: Resurrecting ' + strjoin(sso_dg_path(dg, /name), '-') + string(format=!sso.rwl_format, rwl), /INFORMATIONAL
              ;; This line should be resurrected.  
              parinfo[myidx].pfo.status = !pfo.active
           endif ;; Resurrected a line
        endfor ;; resurrecting lines

        ;; Now I have to decide if I am going to automatically add lines
        ;; as they com into the window.  I vote for no, since the user
        ;; can do that + the graveyard should be a handy resource.
        
        model_spec = pfo_funct(pix_axis, parinfo=parinfo)

        residual = spec - model_spec
        chisq = total(residual^2, /NAN)
        free_idx = where(parinfo.fixed ne 1 and $
                         parinfo.pfo.status eq !pfo.active, nfree)
        redchisq = chisq/(nfree - 1)
;        ;; Make sure we have some sort of error list
;        if N_elements(perrors) ne N_elements(params) then $
;          perrors = dblarr(N_elements(params))
;        
     endelse ;; no good data found in selected wavelength range

     ;; --> I should do a limit check here just to keep mpfit happy
     
     ;; Recalculate model spec for plotting to include regions where
     ;; spectrum is NAN
     model_spec = pfo_funct(pix_axis, parinfo=parinfo)

     wset,6
           
     ;; --> I will want to redo this
;     ssg_plot_fit, pix_axis, params, parinfo, ref_pixel, spec, err_spec, $
;                     title=title, xtitle=xtitle, ytitle=ytitle

     !p.multi = [0,0,2]

     plot, xaxis, spec, title=title, xtitle=xtitle, ytitle=ytitle, $
           xrange=[left_wval, right_wval], yrange=[min(spec), max(spec)], $
           xstyle=1, ystyle=2, psym=!tok.dot
     oploterr, xaxis, spec, err_spec, !tok.dot
     oplot, xaxis, model_spec, linestyle=dotted

     plot, xaxis, residual, $
           title='Fit residual', xtitle=xtitle, ytitle=ytitle, $
           xrange=[left_wval, right_wval], $
           xstyle=1, ystyle=2, psym=!tok.dot
     oploterr, xaxis, residual, err_spec, !tok.dot
     !p.multi = 0
     message, /CONTINUE, 'Use left and right buttons select bracket a region of interest.  Middle button brings up menu.'

     cursor, x1, y1, /DOWN, /DATA
     ;; Left mouse
     if !MOUSE.button eq 1 then begin
        dxs = abs(new_wavelengths - x1)
        junk = min(dxs, a)
        left_idx = a[0]
     endif ;; leftmost mouse button

     ;; Right mouse
     if !MOUSE.button eq 4 then begin
        dxs = abs(new_wavelengths - x1)
        junk = min(dxs, a)
        right_idx = a[0]
     endif ;; rightmost mouse button

     ;; Middle mouse
     if !MOUSE.button eq 2 then begin
        message, /CONTINUE, 'Menu:'
        print, 'Fit '
        print, 'Print plot '
        print, 'reNormalize lines one by one '
        print, 'Modify parameters'
        print, 'unZoom'
        print, 'Save in database'
        print, 'Read from database'
        print, 'Quit'
        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, '[F], P, N, M, R, Z, S, Q?'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'F'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           answer = strupcase(answer)
        endrep until $
          answer eq 'F' or $
          answer eq 'P' or $
          answer eq 'N' or $
          answer eq 'M' or $
          answer eq 'R' or $
          answer eq 'Z' or $
          answer eq 'S' or $
          answer eq 'Q'

        if answer eq 'P' then begin
           pfile=strmid(shortfile, 0, strpos(shortfile, '.fits')) + '.ps'
           message, /CONTINUE, 'Writing postscript file ' + pfile
           set_plot,'ps'
           landscape=1
           device, filename=pfile, landscape=landscape
           ssg_plot_fit, pix_axis, params, parinfo, ref_pixel, spec, err_spec, $
                         title=title, xtitle=xtitle, ytitle=ytitle
           device, /close
           set_plot, 'x'
        endif ;; Print
        if answer eq 'M' then begin
           message, /CONTINUE, 'Modify parameters menu:'
           print, 'Add lines '
           print, 'Remove lines'
           print, 'modify 1 Line'
           print, 'modify Doppler shifts'
           print, 'change dispersion Order'
           print, 'change disperSion relation'
           print, 'change Continuum order'
           print, 'Quit this menu'
           answer = ''
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'A, R, L, D, O, S, C, [Q]'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'Q'
              answer = strupcase(answer)
              for ki = 0,1000 do flush_input = get_kbrd(0)
           endrep until $
             answer eq 'A' or $
             answer eq 'R' or $
             answer eq 'L' or $
             answer eq 'D' or $
             answer eq 'O' or $
             answer eq 'S' or $
             answer eq 'C' or $
             answer eq 'Q'

           ;; Save off exising parameter stuff so we can recover values
;           old_params = params
;           old_parinfo = parinfo

           ;; Change Doppler shifts
           if answer eq 'D' then begin
              idx = where(parinfo.ssgID eq ssgid_dop, count)
              if count gt 0 then begin
                 modpar, idx, params, parinfo
              endif
           endif ;; Change Doppler shifts

           ;; Change Dispersion relation
           if answer eq 'S' then begin
              idx = where(parinfo.ssgID eq ssgid_disp, count)
              if count gt 0 then $
                modpar, idx, params, parinfo
           endif ;; Change dispersion relation

           ;; Change dispersion polynomial order 
           if answer eq 'O' then begin
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'Enter new dispersion polynomial order (9 recalculates dispersion from comp lamp value [' + string(disp_order-1) + ']'
                 answer = get_kbrd(1)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until (byte(answer) ge 48 and byte(answer) le 57) $
                or  byte(answer) eq 10
              if byte(answer) ne 10 then disp_order = (fix(answer)+1) mod 10
              p = 0.d
              disp_idx = where(parinfo.ssgID eq ssgid_disp, old_disp_order)
              ;; Add any param/parinfo entries
              while old_disp_order lt disp_order do begin
                 tparinfo = ssg_init_parinfo()
                 tparinfo[0].limits = [double(p - 10.d), $
                                       double(p + 10.d)]
                 tparinfo[0].parname = $
                                      string(format='("Disp. Coef ", i3)', old_disp_order)
                 tparinfo[0].ssgID = ssgid_disp
                 params = [params, p]
                 parinfo = [parinfo, tparinfo]
                 disp_idx = where(parinfo.ssgID eq ssgid_disp, old_disp_order)
              endwhile
              ;; Delete any param/parinfo entries
              while old_disp_order gt disp_order do begin
                 parinfo[disp_idx[old_disp_order-1]].ssgID = -ssgid_disp
                 disp_idx = where(parinfo.ssgID eq ssgid_disp, old_disp_order)
              endwhile
              good_idx = where(parinfo.ssgID ne -ssgid_disp, count)
              temp = params[good_idx] & params = temp
              temp = parinfo[good_idx] & parinfo = temp

           endif ;; Change dispersion polynomial order 

           ;; Change continuum polynomial order 
           if answer eq 'C' then begin
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'Enter new continuum polynomial order (9 recalculates continuum from spectral median [' + string(N_continuum-1) + ']'
                 answer = get_kbrd(1)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until (byte(answer) ge 48 and byte(answer) le 57) $
                or  byte(answer) eq 10
              if byte(answer) ne 10 then N_continuum = (fix(answer)+1) mod 10
              p = 0.d
              cont_idx = where(parinfo.ssgID eq ssgid_cont, old_N_continuum)
              ;; Add any param/parinfo entries
              while old_N_continuum lt N_continuum do begin
                 tparinfo = ssg_init_parinfo()
                 tparinfo[0].limits = [double(p - 1.d), $
                                       double(p + 1.d)]
                 tparinfo[0].parname = $
                                      string(format='("Cont. Poly Coef ", i3)', old_N_continuum)
                 tparinfo[0].vfID = vfid_cont
                 tparinfo[0].ssgID = ssgid_cont
                 params = [params, p]
                 parinfo = [parinfo, tparinfo]
                 cont_idx = where(parinfo.ssgID eq ssgid_cont, old_N_continuum)
              endwhile
              ;; Delete any param/parinfo entries
              while old_N_continuum gt N_continuum do begin
                 parinfo[cont_idx[old_N_continuum-1]].ssgID = -ssgid_cont
                 cont_idx = where(parinfo.ssgID eq ssgid_cont, old_N_continuum)
              endwhile
              good_idx = where(parinfo.ssgID ne -ssgid_cont, count)
              temp = params[good_idx] & params = temp
              temp = parinfo[good_idx] & parinfo = temp

           endif ;; Change continuum polynomial order 

           ;; Remove lines
           if answer eq 'R' then begin
              message, /CONTINUE, 'Line lists:'
              print, 'Strong solar lines (S) '
              print, 'Broad solar lines (B) '
              print, 'Weak solar lines (W)'
              print, 'O2 atmospheric absoption lines (O)'
              print, 'other Atmospheric absoption lines (A)'
              print, 'atmospheric Emission lines (E)'
              print, 'Io line (I)'
              print, 'Random line by hand (R)'
              print, 'Quit this menu (Q)'
              answer = ''
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'S, B, W, O, A, E, I, R, [Q]'
                 answer = get_kbrd(1)
                 if byte(answer) eq 10 then answer = 'Q'
                 uanswer = strupcase(answer)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until $
                uanswer eq 'S' or $
                uanswer eq 'B' or $
                uanswer eq 'W' or $
                uanswer eq 'O' or $
                uanswer eq 'A' or $
                uanswer eq 'E' or $
                uanswer eq 'I' or $
                uanswer eq 'R' or $
                uanswer eq 'Q'
              
              ;; Strong solar lines
              if uanswer eq 'S' then begin
                 good_idx = where(parinfo.ssggroupID ne 2, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; Broad solar lines
              if uanswer eq 'B' then begin
                 good_idx = where(parinfo.ssggroupID ne 3, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; Weak solar lines
              if uanswer eq 'W' then begin
                 good_idx = where(parinfo.ssggroupID ne 4, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; atmospheric O2 absorption lines
              if uanswer eq 'O' then begin
                 good_idx = where(parinfo.ssggroupID ne 5, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; weak atmospheric absorption lines
              if uanswer eq 'A' then begin
                 good_idx = where(parinfo.ssggroupID ne 6, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; atmospheric emission lines
              if uanswer eq 'E' then begin
                 good_idx = where(parinfo.ssggroupID ne 7, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; Io line
              if uanswer eq 'I' then begin
                 good_idx = where(parinfo.ssggroupID ne 1, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
           endif ;; Remove lines 'R'

           ;; Modify lines
           if answer eq 'L' then begin
              message, /CONTINUE, 'Use left mouse button select line'
              cursor, x1, y1, /DOWN, /DATA
              ;; Left mouse
              if !MOUSE.button eq 1 then begin
                 ;; Find the observed line center closest to the mouse
                 ;; click + expresss it as a scaler, not an array
                 dxs = abs(parinfo.ssgowl - x1)
                 junk = min(dxs, a)
                 lc_idx = a[0]
                 ;; Now get the defining parameters for the line so
                 ;; that we can grab all of its parameters.  --> This
                 ;; will be useful in the generalized graveyard code
                 rwl = parinfo[lc_idx].ssgrwl
                 group = parinfo[lc_idx].ssggroupID
                 line_idx = where(parinfo.ssggroupID eq group and $
                                  parinfo.ssgrwl eq rwl)
                 modpar, line_idx, params, parinfo
              endif             ; left mouse button
           endif ;; Modify lines 'L'

           ;; Add lines
           if answer eq 'A' then begin
              message, /CONTINUE, 'Line lists:'
              print, 'Strong solar lines (S) '
              print, 'Broad solar lines (B) '
              print, 'Weak solar lines (W)'
              print, 'O2 atmospheric absorption lines (O)'
              print, 'weak Atmospheric absorption lines (A)'
              print, 'atmospheric Emission lines (E)'
              print, 'Io line (I)'
              print, 'Random line by hand (R)'
              print, 'Quit this menu (Q or q)'
              answer = ''
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'S, B, W, O, A, E, I, R, [Q]'
                 answer = get_kbrd(1)
                 if byte(answer) eq 10 then answer = 'Q'
                 uanswer = strupcase(answer)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until $
                uanswer eq 'S' or $
                uanswer eq 'B' or $
                uanswer eq 'W' or $
                uanswer eq 'O' or $
                uanswer eq 'A' or $
                uanswer eq 'E' or $
                uanswer eq 'I' or $
                uanswer eq 'R' or $
                uanswer eq 'Q'
              
              if uanswer eq 'S' or uanswer eq 'B' or uanswer eq 'W' then begin
                 ;; Check to see if we need to add a solar doppler
                 ;; shift parameter
                 sd_idx = where(parinfo.ssgID eq ssgid_dop $
                                and parinfo.ssgdop eq id_solar_dop, count)
                 if count eq 0 then begin
                    tparam=dopplers[id_solar_dop]
                    tparinfo = ssg_init_parinfo(1)
                    ;; --> Again, a wild guess, assuming we are in
                    ;; km/s, etc.
                    tparinfo.limits = [tparam-2,tparam+2]
                    tparinfo.parname = 'Solar Doppler Shift'
                    tparinfo.ssgID = ssgid_dop
                    tparinfo.ssgdop = id_solar_dop
                    if tparam eq 0 then begin
                       message, 'WARNING: solar doppler shift was not automatically detected, please supply', /CONTINUE
                       modpar, 0, tparam, tparinfo
                    endif
                    params = [params, tparam]
                    parinfo = [parinfo, tparinfo]
                 endif

              endif ;; adding some solar lines, strong or weak

              if uanswer eq 'I' then begin
                 ;; Check to see if we need to add an Io doppler
                 ;; shift parameter
                 iod_idx = where(parinfo.ssgID eq ssgid_dop $
                                 and parinfo.ssgdop eq id_Io_dop, count)
                 if count eq 0 then begin
                    tparam=dopplers[id_Io_dop]
                    tparinfo = ssg_init_parinfo(1)
                    ;; --> Again, a wild guess, assuming we are in
                    ;; km/s, etc.
                    tparinfo.limits = [tparam-2,tparam+2]
                    tparinfo.parname = 'Io Doppler Shift'
                    tparinfo.ssgID = ssgid_dop
                    tparinfo.ssgdop = id_Io_dop
                    if tparam eq 0 then begin
                       message, 'WARNING: Io doppler shift was not automatically detected, please supply', /CONTINUE
                       modpar, 0, tparam, tparinfo
                    endif
                    params = [params, tparam]
                    parinfo = [parinfo, tparinfo]
                 endif

                 Io_idx = where(left_dop_wvals[id_Io_dop] le io_lines and $
                                io_lines le right_dop_wvals[id_Io_dop], $
                                n_Io)
                 ;; For each Io line
                 for iil = 0, n_Io-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = io_lines[Io_idx[iil]]
                    old_idx = where(old_parinfo.ssggroupID eq 1 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       wave_err = 5.d ; I end up not using this at the
                                ; moment since I want all uncertainty
                                ; reflected in the Doppler shift (see .fixed)
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, 2., 80.]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].fixed = 1
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-wave_err, wave_err]
                       tparinfo[1].limited = [1,0]
                       tparinfo[1].limits  = [0.d, 100]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [10.d, 300.d]
                       tparinfo[3].limited = [1,1]
                       tparinfo[3].limits  = [0.d, 10.d]
                       tparinfo[*].ssggroupID = 1
                       tparinfo[0].ssgdop = id_Io_dop
                       for ipn=0,3 do begin
                          tparinfo[ipn].parname = $
                            string(group_names[tparinfo[ipn].ssggroupID], $
                                   ' ', tparinfo[ipn].parname)
                       endfor
                       tparams[0] = 0.D
                       params = [params, tparams]
                       parinfo = [parinfo, tparinfo]
                    endif ;; Initialize a Io line
                 endfor ;; Io line loop
              endif ;; adding Io line

              ;; Strong solar lines
              if uanswer eq 'S' then begin
                 solar_idx = $
                            where(left_dop_wvals[id_solar_dop] le $
                                  solar_atlas_air_waves[*] and $
                                  solar_atlas_air_waves[*] le $
                                  right_dop_wvals[id_solar_dop], $
                                  n_solar)
                 ;; For each strong solar line
                 for isl = 0, n_solar-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = solar_atlas_air_waves[solar_idx[isl]]
                    old_idx = where(old_parinfo.ssggroupID eq 2 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line
                       wave_err = solar_atlas.e_lambdaF[solar_idx[isl]]*1000d
                       dop_width = 200
                       ew = -continuum/8
                       ;; Use Meylan's fits to improve equivalent width
                       match_idx = where(weak_matched_waves eq wl, count)
                       if count gt 0 then ew = -meylan_weak.ew[match_idx[0]]
                       match_idx = where(strong_matched_waves eq wl, count)
                       if count gt 0 then ew = -meylan_strong.ew[match_idx[0]]
;                       ;; Take a wild guess at the initial parameters
;                       area = -continuum[0]*dop_width /8
;                       area = -exp(solar_atlas.loggf[solar_idx[isl]] - 12) $
;                              * continuum[0] * 30
;                       area = -exp(solar_atlas.loggf[solar_idx[isl]]) $
;                              * continuum[0] /5.
;                       area = -solar_atlas.I[solar_idx[isl]] * $
;                              continuum[0]/1000.
                       tparams = [wl, ew, dop_width]
                       
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-wave_err, wave_err]
                       tparinfo[1].limited = [0,1]
                       tparinfo[1].limits  = [2.d*ew, 0.d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [20.d, 300.d]
                       tparinfo[3].limited = [1,1]
                       tparinfo[3].limits  = [0.d, 250.d]
                       tparinfo[*].ssggroupID = 2
                       tparinfo[0].ssgdop = id_solar_dop
                       for ipn=0,3 do begin
                          tparinfo[ipn].parname = $
                            string(group_names[tparinfo[ipn].ssggroupID], $
                                   ' ', tparinfo[ipn].parname)
                       endfor
                       tparams[0] = 0.D
                       params = [params, tparams]
                       parinfo = [parinfo, tparinfo]
                    endif ;; Initialize a solar line
                 endfor ;; Strong solar line loop
              endif ;; adding strong solar line

              ;; Broad solar lines
              if uanswer eq 'B' then begin
                 solar_idx = where(left_dop_wvals[id_solar_dop] le $
                                   broad_solar_lines and $
                                   broad_solar_lines le $
                                   right_dop_wvals[id_solar_dop], $
                                   n_solar)
                 ;; For each broad solar line
                 for isl = 0, n_solar-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = broad_solar_lines[solar_idx[isl]]
                    ew = broad_solar_line_EWs[solar_idx[isl]]
                    old_idx = where(old_parinfo.ssggroupID eq 3 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       wave_err = 50.d
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, ew, 200.]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-wave_err, wave_err]
                       tparinfo[1].limited = [1,1]
                       tparinfo[1].limits  = [-100d, 0.d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [20.d, 1000.d]
                       tparinfo[3].fixed   = 0
                       tparinfo[3].limited = [1,1]
                       tparinfo[3].limits  = [0.d, 1000.d]
                       tparinfo[*].ssggroupID = 3
                       tparinfo[0].ssgdop = id_solar_dop
                       for ipn=0,3 do begin
                          tparinfo[ipn].parname = $
                            string(group_names[tparinfo[ipn].ssggroupID], $
                                   ' ', tparinfo[ipn].parname)
                       endfor

                       tparams[0] = 0.D
                       params = [params, tparams]
                       parinfo = [parinfo, tparinfo]
                    endif ;; Initialize a solar line
                 endfor ;; Broad solar line loop
              endif ;; adding broad solar line

              ;; Weak solar lines
              if uanswer eq 'W' then begin
                 solar_idx = where(left_dop_wvals[id_solar_dop] le $
                                   weak_solar_lines and $
                                   weak_solar_lines le $
                                   right_dop_wvals[id_solar_dop], $
                                   n_solar)
                 ;; For each weak solar line
                 for isl = 0, n_solar-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = weak_solar_lines[solar_idx[isl]]
                    old_idx = where(old_parinfo.ssggroupID eq 4 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       wave_err = 5.d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, -2., 100.]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-wave_err, wave_err]
                       tparinfo[1].limited = [1,1]
                       tparinfo[1].limits  = [-3d, 0.d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [20.d, 200.d]
                       tparinfo[3].fixed   = 1
                       tparinfo[3].limited = [1,1]
                       tparinfo[3].limits  = [0.d, 250.d]
                       tparinfo[*].ssggroupID = 4
                       tparinfo[0].ssgdop = id_solar_dop
                       for ipn=0,3 do begin
                          tparinfo[ipn].parname = $
                            string(group_names[tparinfo[ipn].ssggroupID], $
                                   ' ', tparinfo[ipn].parname)
                       endfor

                       tparams[0] = 0.D
                       params = [params, tparams]
                       parinfo = [parinfo, tparinfo]
                    endif ;; Initialize a solar line
                 endfor ;; Weak solar line loop
              endif ;; adding weak solar line

              ;; O2 atmospheric absorption lines
              if uanswer eq 'O' then begin
                 o2_idx = where(left_wval le o2_air_waves and $
                                o2_air_waves le right_wval, $
                                n_o2)
                 ;; For absoprtion feature
                 for ial = 0, n_o2-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = o2_air_waves[o2_idx[ial]]
                    old_idx = where(old_parinfo.ssggroupID eq 5 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       wave_err = 5.d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, -20, 100.]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-wave_err, wave_err]
                       tparinfo[1].limited = [0,1]
                       tparinfo[1].limits  = [-200.D, 0]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [5.d, 300.d]
                       tparinfo[3].limited = [1,1]
                       tparinfo[3].limits  = [0.d, 250.d]
                       tparinfo.ssggroupID[*] = 5
                       tparinfo[0].ssgdop = 0
                       for ipn=0,3 do begin
                          tparinfo[ipn].parname = $
                            string(group_names[tparinfo[ipn].ssggroupID], $
                                   ' ', tparinfo[ipn].parname)
                       endfor
                       tparams[0] = 0.D
                       params = [params, tparams]
                       parinfo = [parinfo, tparinfo]
                    endif ;; Initialize a strong atmospheric line
                 endfor ;; Strong atmospheric
              endif ;; adding strong atmospheric line

              ;; weak atmospheric absorption lines
              if uanswer eq 'A' then begin
                 atm_idx = where(left_wval le weak_atm_lines and $
                                weak_atm_lines le right_wval, $
                                n_atm)
                 ;; For absoprtion feature
                 for ial = 0, n_atm-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = weak_atm_lines[atm_idx[ial]]
                    old_idx = where(old_parinfo.ssggroupID eq 6 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       wave_err = 50.d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, -2, 100]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-wave_err, wave_err]
                       tparinfo[1].limited = [0,1]
                       tparinfo[1].limits  = [-10.D, 0]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [5.d, 300.d]
                       tparinfo[3].limited = [1,1]
                       tparinfo[3].limits  = [0.d, 250.d]
                       tparinfo.ssggroupID[*] = 6
                       tparinfo[0].ssgdop = 0
                       for ipn=0,3 do begin
                          tparinfo[ipn].parname = $
                            string(group_names[tparinfo[ipn].ssggroupID], $
                                   ' ', tparinfo[ipn].parname)
                       endfor
                       tparams[0] = 0.D
                       params = [params, tparams]
                       parinfo = [parinfo, tparinfo]
                    endif ;; Initialize a weak atmospheric line
                 endfor ;; weak atmospheric
              endif ;; adding a weak atm atmospheric line

              ;; Atmospheric emission lines
              if uanswer eq 'E' then begin
                 atm_idx = where(left_wval le atm_emi and $
                                 atm_emi le right_wval, $
                                 n_atm)
                 ;; For each atmospheric line
                 for ial = 0, n_atm-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = atm_emi[atm_idx[ial]]
                    old_idx = where(old_parinfo.ssggroupID eq 7 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       wave_err = 5.d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, 2, 50]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-wave_err, wave_err]
                       tparinfo[1].limited = [1,0]
                       tparinfo[1].limits  = [0,2.d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [5.d, 300.d]
                       tparinfo[3].limited = [1,1]
                       tparinfo[3].limits  = [0.d, 250.d]
                       tparinfo.ssggroupID[*] = 7
                       tparinfo[0].ssgdop = 0
                       for ipn=0,3 do begin
                          tparinfo[ipn].parname = $
                            string(group_names[tparinfo[ipn].ssggroupID], $
                                   ' ', tparinfo[ipn].parname)
                       endfor

                       tparams[0] = 0.D
                       params = [params, tparams]
                       parinfo = [parinfo, tparinfo]
                    endif ;; Initialize an atmospheric emission line
                 endfor ;; Atmospheric emission
              endif ;; adding atmospheric emission line

           endif ;; Adding lines menu of modifying parameters

           ;; Last thing: display parameters
           ssg_display_params, params, parinfo, perrors
           ;; Reset answer so it doesn't trigger other menus
           answer = ''
        endif ;; Modify parameters 'M'

        if answer eq 'F' then begin
           params = parinfo.value
           err=0
           CATCH, err
           if err ne 0 then begin
              message, /NONAME, !error_state.msg, /CONTINUE
              message, 'WARNING: error detected, reseting parameters to previous values', /CONTINUE
              params =  parinfo.value
           endif else begin

              to_pass = {parinfo:parinfo}
              params = mpfitfun('pfo_funct', pix_axis, spec, err_spec, $
                                parinfo=parinfo, $
                                functargs=to_pass, autoderivative=1, $
                                iterproc='pfo_iterproc', perror=perror, $
                                status=status)



;              to_pass = {parinfo:parinfo, ref_pixel:ref_pixel, spec:spec, $
;                         err_spec:err_spec}
;              params = mpfitfun('io_spec', pix_axis, spec, err_spec, $
;                                params, FUNCTARGS=to_pass, AUTODERIVATIVE=1, $
;                                PARINFO=parinfo, maxiter=maxiter, $
;                                BESTNORM=chisq, PERROR=perrors, STATUS=status, $
;                                QUIET=quiet, NPRINT=nprint)
;
;              ssg_display_params, params, parinfo, perrors
              message, 'MPFITFUN returned STATUS ' + string(status), /CONTINUE
              io_idx = where(parinfo.ssggroupID eq 1, count)
              if count eq 0 then begin
                 message, /continue, 'WARNING: No Io parameters'
              endif else begin
                 dop_idx = where(parinfo.ssgID eq ssgid_dop and $
                                 parinfo.ssgdop eq id_Io_dop)
                 message, 'Io Doppler shift delta is ' + string(deldots[0]-params[dop_idx[0]]) + '+/-' + string(perrors[dop_idx[0]]), /CONTINUE
                 if abs(params[dop_idx[0]]) lt 5 then begin
                    message, /continue, 'WARNING: Io is close to the airglow'
                    air_idx = where(parinfo.ssggroupID eq 7, count)
                    if count eq 0 then begin
                       message, /CONTINUE, 'WARNING: No airglow line defined'
                    endif else begin
                       ssg_display_params, params[air_idx], parinfo[air_idx], $
                                           perrors[air_idx]
                    endelse
                 endif
              endelse
              dop_idx = where(parinfo.ssgID eq ssgid_dop and $
                              parinfo.ssgdop eq id_solar_dop, count)
              if count gt 0 then begin
                 message, 'Solar Doppler delta is ' + string(deldots[0]+rdots[0]-params[dop_idx[0]]) + '+/-' + string(perrors[dop_idx[0]]), /CONTINUE
              endif
              weak_idx = where(parinfo.ssggroupID eq 4, count)
              if count gt 0 then begin
                 message, /CONTINUE, 'NOTE: keep an eye on these Weak solar lines'
                 ssg_display_params, params[weak_idx], parinfo[weak_idx], $
                                     perrors[weak_idx]
              endif
              if status eq 5 then $
                message, 'WARNING: STATUS 5 means MPFITFUN failed to converge after MAXITER (' + string(maxiter) + ') iterations', /CONTINUE
           endelse
           CATCH, /CANCEL
           did_fit = 1
           saved = 0
        endif ;; Fit

        ;; Renormalize
        if answer eq 'N' then begin
           old_params = params
           old_parinfo = parinfo
           err=0
           CATCH, err
           if err ne 0 then begin
              message, /NONAME, !error_state.msg, /CONTINUE
              message, 'WARNING: error detected, reseting parameters to previous values', /CONTINUE
              params =  old_params
              parinfo = old_parinfo
           endif
           ;; Fix everything but the line strengths
           old_fixed = parinfo.fixed
           all_but_ews = where(parinfo.vfID ne 5, complement=ew_idx)
           parinfo[all_but_ews].fixed = 1

           ;; Now for each line do an mpfitfun
           for il=0, n_elements(ew_idx)-1 do begin
              parinfo[ew_idx].fixed = 1
              parinfo[ew_idx[il]].fixed = 0
              to_pass = {parinfo:parinfo, ref_pixel:ref_pixel, spec:spec, $
                         err_spec:err_spec}
              params = mpfitfun('io_spec', pix_axis, spec, err_spec, $
                                params, FUNCTARGS=to_pass, AUTODERIVATIVE=1, $
                                PARINFO=parinfo, maxiter=10, QUIET=quiet, NPRINT=nprint)
           endfor
           parinfo.fixed = old_fixed
           CATCH, /CANCEL
           did_fit = 1
           saved = 0
        endif ;; Renormalize

        if answer eq 'Z' then begin
           left_idx = 0
           right_idx = nx-1
        endif
        if answer eq 'Q' then begin
           if did_fit and NOT saved then begin
              message, 'WARNING: did a fit but parameters not saved', /CONTINUE
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'Exit without saving? (Y, [N])'
                 answer = get_kbrd(1)
                 if byte(answer) eq 10 then answer = 'N'
                 answer = strupcase(answer)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until $
                answer eq 'Y' or $
                answer eq 'N'
              if answer eq 'Y' then done = 1
           endif else begin
              done = 1
           endelse
        endif ;; QUIT

        if answer eq 'S' then begin
           oldpriv=!priv
           !priv = 2
           ;; First get the multi-valued variable's sizes
           dbopen, fdbname, 0
           entry = where_nday_eq(nday, COUNT=count,SILENT=silent, $
                                 tolerance=0.002) ; in case we were called by hand

           dbext, entry, 'value, perror, llimited, rlimited, llimits, rlimits, vfID, ssgID, ssggroupID, ssgrwl, ssgowl, ssgdop', $
                  values	, $
                  dbperrors	, $ 
                  llimited   , $  
                  rlimited   , $  
                  llimits    , $
                  rlimits    , $
                  vfID       , $
                  ssgID      , $
                  ssggroupID , $
                  ssgrwl     , $
                  ssgowl     , $
                  ssgdop
           dbext, entry, 'fit_vers, fit_date, new_spec', $
                  fit_vers, fit_date, new_spec
           dbclose

           new_spec = 0
           get_date, today
           fit_date = today
           ;; keep the type consistent
           fit_vers=fit_vers+byte(1)
           
           n_params = N_elements(params)
           to_save=ssg_init_parinfo(N_elements(values))

           values [0:n_params-1]            = params[*]
           dbperrors[0:n_params-1]          = perrors[*]
           to_save[0:n_params-1].limited    =  parinfo.limited   
           to_save[0:n_params-1].limits     =  parinfo.limits    
           to_save[0:n_params-1].parname    =  parinfo.parname   
           to_save[0:n_params-1].tied       =  parinfo.tied      
           to_save[0:n_params-1].vfID       =  parinfo.vfID      
           to_save[0:n_params-1].ssgID      =  parinfo.ssgID     
           to_save[0:n_params-1].ssggroupID =  parinfo.ssggroupID
           to_save[0:n_params-1].ssgrwl     =  parinfo.ssgrwl    
           to_save[0:n_params-1].ssgowl     =  parinfo.ssgowl    
;;           to_save[0:n_params-1].ssglink    =  parinfo.ssglink   
           to_save[0:n_params-1].ssgdop     =  parinfo.ssgdop    

           ;; Sub-vector structure are hard to deal with, so separate
           ;; them out
           llimited[0:n_params-1] = to_save[0:n_params-1].limited[0]
           rlimited[0:n_params-1] = to_save[0:n_params-1].limited[1]
           llimits [0:n_params-1]=  to_save[0:n_params-1].limits[0]
           rlimits [0:n_params-1]=  to_save[0:n_params-1].limits[1]

           ;; Our flag for the end of the list
           to_save[n_params].ssgID = -1

           ;; Must do updates one file at a time
           dbopen, fdbname, 1
           dbupdate, entry, 'value, perror, llimited, rlimited, llimits, rlimits, vfID, ssgID, ssggroupID, ssgrwl, ssgowl, ssgdop', $
                     values		, $
                     dbperrors		, $
                     llimited		, $  
                     rlimited		, $
                     llimits		, $  
                     rlimits		, $
                     to_save.vfID       , $
                     to_save.ssgID      , $
                     to_save.ssggroupID , $
                     to_save.ssgrwl     , $
                     to_save.ssgowl     , $
                     to_save.ssgdop
           dbupdate, entry, 'fit_vers, fit_date, new_spec, nfree, chisq, redchisq', fit_vers, fit_date, new_spec, fix(nfree), chisq, redchisq
           dbclose
;            dbopen, fdbname[1], 1
;            ;; I think I have to pad these for them to work properly
;            ;; in the array code
;            for idbp = 0,N_elements(to_save)-1 do begin
;               while strlen(to_save[idbp].parname) lt 20 do $
;                 to_save[idbp].parname = to_save[idbp].parname + ' '
;               while strlen(to_save[idbp].tied) lt 20 do $
;                 to_save[idbp].tied = to_save[idbp].tied + ' '
;            endfor
;            dbupdate, entry, 'parname, tied', to_save.parname, to_save.tied

           !priv=oldpriv
           dbclose
           message, /CONTINUE, 'Saved parameters for ' +  shortfile + ',nday = ' + string(nday)
           saved = 1
        endif ;; Save to database

        ;; Read from database
        if answer eq 'R' then begin
           dbclose
           good_nday = ssg_select([nday-0.5, nday+0.5])
           if good_nday ne -1 then begin
              dbopen, fdbname, 0
              entry = where_nday_eq(good_nday, COUNT=count,SILENT=silent) 
              if count ne 1 then $
                message, 'ERROR: database not consistant, or something'
              dbext, entry, 'value, llimited, rlimited, llimits, rlimits, vfID, ssgID, ssggroupID, ssgrwl, ssgowl, ssgdop', $
                     values, $
                     llimited, $
                     rlimited, $
                     llimits, $
                     rlimits, $
                     vfID, $
                     ssgID, $
                     ssggroupID, $
                     ssgrwl, $
                     ssgowl, $
                     ssgdop
              dbclose
;              dbopen, fdbname[1], 0
;              dbext, entry, 'parname, tied', parname, tied

              nparams=0
              ;; Our flag for the end of the list
              while ssgID[nparams] ne -1 and $
                nparams lt N_elements(ssgID) - 1 do $
                nparams = nparams + 1
              if nparams eq N_elements(ssgID)-1 then begin
                 message, 'WARNING: you selected a spectrum that has no recorded fit.  Not modifying parameters', /CONTINUE
              endif else begin

                 params = dblarr(nparams)
                 params = values[0:nparams-1]

                 parinfo=ssg_init_parinfo(nparams)

                 parinfo.limited[0] = llimited   [0:nparams-1]
                 parinfo.limited[1] = rlimited   [0:nparams-1]
                 parinfo.limits[0]  = llimits    [0:nparams-1]
                 parinfo.limits[1]  = rlimits    [0:nparams-1]
;                 parinfo.parname    = parname    [0:nparams-1]
;                 parinfo.tied       = tied       [0:nparams-1]
                 parinfo.vfID       = vfID       [0:nparams-1]
                 parinfo.ssgID      = ssgID      [0:nparams-1]
                 parinfo.ssggroupID = ssggroupID [0:nparams-1]
                 parinfo.ssgrwl     = ssgrwl     [0:nparams-1]
                 parinfo.ssgowl     = ssgowl     [0:nparams-1]
;;                 parinfo.ssglink    = ssglink    [0:nparams-1]
                 parinfo.ssgdop     = ssgdop     [0:nparams-1]

                 ;; Have to redo the labels by hand.
                 disp_order = 0
                 N_continuum = 0
                 for ip=0,nparams-1 do begin
                    case parinfo[ip].ssgID of
                       ssgid_disp : begin
                          parinfo[ip].parname = $
                                               string(format='("Disp Coef ", i3)', disp_order)
                          disp_order = disp_order + 1
                       end
                       ssgid_cont : begin
                          parinfo[ip].parname = $
                                               string(format='("Cont. Poly Coef ", i3)', $
                                                      N_continuum)
                          N_continuum = N_continuum + 1                        
                       end
                       ssgid_dop : begin
                          parinfo[ip].parname = $
                                               string(doppler_names[parinfo[ip].ssgdop], $
                                                      ' doppler shift') 
                       end
                       ssgid_voigt : begin
                          parinfo[ip].parname = $
                                               string(format='(a, (f10.4), " ", a)', $
                                                      group_names[parinfo[ip].ssggroupID], $
                                                      parinfo[ip].ssgrwl, $
                                                      vpnames[parinfo[ip].vfID-2])
                          
                       end
                       else : parinfo[ip].parname = 'Unknown'
                    endcase
                    
                 endfor

              endelse ;; Good nday found
           endif ;; Fit information found
           dbclose
        endif ;; Read from database

     endif ;; Middle mouse button

  endrep until done

end
