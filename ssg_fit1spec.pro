;+

; $Id: ssg_fit1spec.pro,v 1.7 2004/06/25 00:55:24 jpmorgen Exp $

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

pro ssg_fit1spec, nday, obj, N_continuum=N_continuum_in, $
                  disp_min_ew=disp_min_ew, maxiter=maxiter, quiet=quiet, $
                  grave_report=grave_report, nprint=nprint, min_ew=min_ew, $
                  xtol=xtol, ftol=ftol, gtol=gtol, resdamp=resdamp, $
                  mpstep=mpstep, landscape=landscape

  init = {ssg_sysvar}

  if N_elements(nday) ne 1 then $
    message, 'ERROR: supply one and only nday at a time'
  if nday eq -1 then begin
     message, 'NOTE: encountered nday = -1 flag, terminating', /INFORMATIONAL
     return
  endif
  if N_elements(obj) eq 0 then obj = !eph.io
  if N_elements(nprint) eq 0 then nprint=10
  if N_elements(maxiter) eq 0 then maxiter = 100
  if N_elements(grave_report) eq 0 then grave_report = 10
  if N_elements(min_ew) eq 0 then min_ew = 0.5
  if N_elements(disp_min_ew) eq 0 then disp_min_ew = 10
  if N_elements(ftol) eq 0 then ftol=1d-5
  if N_elements(xtol) eq 0 then xtol=1d-10
  if N_elements(gtol) eq 0 then gtol=1d-10
  if N_elements(resdamp) eq 0 then resdamp=0
  if N_elements(mpstep) eq 0 then mpstep=0
  if N_elements(landscape) eq 0 then landscape=1

  if N_elements(N_continuum_in) eq 0 then N_continuum_in = 3
  N_continuum_orig = N_continuum_in

  rdbname = 'ssg_reduce'

  dbopen, rdbname, 0
  rentry = where_nday_eq(nday, COUNT=count,SILENT=silent, tolerance=0.001)
  if count eq 0 then message, $
    'ERROR: nday ' + strtrim(nday,2) + ' not found in ' + rdbname

  dbext, rentry, 'nday, dir, fname, object, date, time, bad', ndays, dirs, files, objects, dates, times, badarray

  dbext, rentry, 'wavelen, spectrum, spec_err, cross_disp, cross_err, disp_pix, dispers', wavelengths, spectra, spec_errors, cross_disps, cross_errors, disp_pix, orig_dispers

  dbclose
  
  dirs = strtrim(dirs)
  files=strtrim(files)
  objects=strtrim(objects)
  shortfile= $
    strmid(files[0], strpos(files[0], '/', /REVERSE_SEARCH) + 1)
  sparinfo_fname = dirs[0] + '/sparinfo_' + strtrim(round(ndays[0]), 2) + '.sav'
  ;; --> I am going to want to think about this more: the exact role
  ;; of the fitting database.  I think I want to call it ssg_fit
  fdbname = 'oi_6300_fit'

  message, /INFO, 'fitting spectrum nday = ' + strtrim(nday, 2)

;
;  ;; Check on the source of these and add to it.  These come from
;  ;; Melanie.
;  weak_solar_lines = [6286.3d, $
;                      6286.64d, $
;                      6298.97d, $
;                      6301.0d, $
;                      6301.86d]
;
;  ;; Doing a residual analysis on 10/14/03.  Weak is no longer the
;  ;; right word for the feature to the blue of 6299.5957.  Since I am
;  ;; treating this separately, I might as well do the whole line
;  ;; complex.  Try putting a line the same distance away from the
;  ;; 6301.5115 line
;  broad_solar_lines = [6299.29d, 6299.5957d, 6301d]
;  broad_solar_line_EWs = [-5, -40, -5]
;
;  ;; This is the atmospheric line list that Melanie was using.  
;;  atm_absorb = [6298.457d, 6299.228d, 6302.0001d, 6302.7642d];
;
;; I am not sure where this one came from: , 6304.53d
;
;  ;; I got the weak atm absorption lines by fitting residuals.
;  ;; Sometimes these are not so weak!
;  weak_atm_lines = [6290.85d, 6292.575d, 6294.675d]
  

  ;; Check my ephemeris code against what Melanie and Divia downloaded
  dbopen,'io6300_integrated',0  
  io6300_entry = $
    where_nday_eq(nday, COUNT=count,SILENT=silent, tolerance=0.002)

  if count gt 1 then begin
     eph_deldot = sso_eph_dop(nday2date(nday), [obj, !eph.earth], $
                            !ssg.mmp_xyz)
     eph_double = sso_eph_dop(nday2date(nday), [!eph.sun, obj, !eph.earth], $
                            !ssg.mmp_xyz)
     dbext, io6300_entry, 'deldot, rdot', deldots, rdots
     dbclose  

     if abs(eph_deldots - deldots) gt 0.5 then $
       message, 'ERROR: ephemeris values for Io-Earth velocity differ by more than 0.5 km/s'
     if abs(eph_double - (deldots + rdots)) gt 0.5 then $
       message, 'ERROR: ephemeris values for Sun-Io-Earth velocity differ by more than 0.5 km/s'

  endif

  if median(spectra[*,0]) lt !ssg.min_cont then $
    message, 'ERROR: this is not a continuum spectrum.  Code not written yet to handle this.'


  ;; Build up a large parinfo from which we can choose lines to fit

  obj_path = sso_path_create([obj, !eph.earth])
  obj_dop = sso_eph_dop(nday2date(nday), obj_path, !ssg.mmp_xyz)
  sun_obj_path = sso_path_create([!eph.sun, obj, !eph.earth])
  sun_obj_dop = sso_eph_dop(nday2date(nday), sun_obj_path, !ssg.mmp_xyz)

  value = obj_dop
  deldot_par = $
    pfo_fcreate(!pfo.sso_funct, ptype=!sso.dop, path=obj_path, $
                step=mpstep, value=value, limited=[1,1], $
                limits=[value-5, value+5], $
                format=['f8.3'], eformat=['f6.2'], $
                parinfo_template=!ssg.parinfo)

  value = sun_obj_dop
  rdot_par   = $
    pfo_fcreate(!pfo.sso_funct, ptype=!sso.dop, path=sun_obj_path, $
                step=mpstep, value=value, limited=[1,1], $
                limits=[value-5, value+5], $
                format=['f8.3'], eformat=['f6.2'], $
                parinfo_template=!ssg.parinfo)

  ;; Read initial line parinfo into *!ssg.lparinfo (if it isn't there
  ;; already) and transfer it to a local lparinfo.
  ssg_lparinfo, [min(wavelengths[*,0], /NAN), max(wavelengths[*,0], /NAN)]
  lparinfo = *!ssg.lparinfo
  sso_dg_assign, lparinfo

  ;; Convert generic object ID code to desired object code.  Also,
  ;; choose a reference point for the continuum polynomial.  Default
  ;; to the middle of the wavelength range unless we have object
  ;; lines, in which case, use the middle of them.
  cont_poly_ref = mean([min(wavelengths[*,0], /NAN), max(wavelengths[*,0], /NAN)])
  obj_dg = sso_path_dg(sso_path_create([!eph.obj, !eph.earth]))
  obj_idx = where(lparinfo.sso.dg eq obj_dg, count)
  if count eq 0 then begin
     message, 'WARNING: no object lines found in the passband', /CONTINUE
  endif else begin
     lparinfo[obj_idx].sso.path = obj_path
     ;; This weights lines with more parameters more highly
     cont_poly_ref = mean(lparinfo[obj_idx].sso.rwl, /NAN)
  endelse

  sun_obj_dg = sso_path_dg(sso_path_create([!eph.sun, !eph.obj, !eph.earth]))
  sun_obj_idx = where(lparinfo.sso.dg eq sun_obj_dg, count)
  if count eq 0 then $
    message, 'WARNING: no solar lines found in the passband', /CONTINUE
  lparinfo[sun_obj_idx].sso.path = sun_obj_path
  ;; Reassign dgs to new paths
  sso_dg_assign, lparinfo
    
  ;; Pull out the object and airglow lines for indication in sso_plot_fit
  obj_dg = sso_path_dg(obj_path)
  ag_dg = sso_path_dg(sso_path_create([!eph.earth, !eph.earth]))
  special_idx = where(lparinfo.sso.dg eq obj_dg, nio)
  if nio gt 0 then begin
     ag_idx = where(lparinfo.sso.dg eq ag_dg and $
                    lparinfo.sso.ttype eq !sso.ew and $
                    lparinfo.value gt 0, count)
     ;; Put all airglow lines into the special list
     for iag=0, count-1 do begin
        rwl = lparinfo[ag_idx[iag]].sso.rwl
        dg  = lparinfo[ag_idx[iag]].sso.dg
        myidx = where(lparinfo.sso.rwl eq rwl and $
                      lparinfo.sso.dg eq dg)
        special_idx = [special_idx, myidx]
     endfor ;; each airglow line
     specials = lparinfo[special_idx]
     !sso.special_lines = ptr_new(specials, /no_copy)
  endif ;; Io line(s) in lparinfo


  ;; This depends on having the reduced files around
  cd, dirs[0]
  im=ssgread(files[0], hdr, /DATA)
  asize = size(im) & nx = asize(1) & ny = asize(2)

  ;; --> TEMPORARY CODE.  I am going to nuke this after I switch over
  ;; to using the dispersion in the database.
  ;; Read in dispersion coefficients so I can use that as part of the
  ;; fit.  This code is replaced below by reading the database.
  disp_order = 0
  orig_dispers = $
    sxpar(hdr, string(format='("DISPERS", i1)', disp_order), count=count)
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
    message, 'ERROR: not enough DISPERS keywords found in header of ' + shortfile


  window,6
  title = string(objects[0], ' ', shortfile, ' ', nday2date(ndays[0]), ' (UT)')
  ;; This has to be in font !3 for the angstrom symbol to be found.
  ;; The extra ;" is to close the " in the string
  xtitle = 'Rest Wavelength, '+string("305B) ;" ;
  ytitle = string('Signal (', sxpar(hdr, 'BUNIT'), '/S)')
  
  good_pix = where(finite(disp_pix[*,0]) eq 1 and $
                   finite(wavelengths[*,0]) eq 1 and $
                   finite(spectra[*,0]) eq 1 and $
                   finite(spec_errors[*,0]) eq 1, n_pix)
  if n_pix eq 0 then begin
     message, /continue, 'ERROR: no good data found in ' + files[0]
     return
  endif

  ;; Establish a figure of merit for small equivalent widths so that
  ;; Lorentzian widths can be set to 0.  This is based on the
  ;; intrinsic noise in the data (Carey Woodward's trick).
  temp = spectra[good_pix[1:n_pix-1]] - spectra[good_pix[0:n_pix-2]]
  ;; small_ew = abs(median(temp) * orig_dispers[1] / !sso.lwcvt)
  ;; Try using the error bar instead
  small_ew = median(spec_errors[good_pix, 0]) * orig_dispers[1] / $
             !sso.lwcvt

  left_wval = min(wavelengths, a)
  left_pix = a[0]
  right_wval = max(wavelengths, a)
  right_pix = a[0]

  ref_pixel = nx/2.

  done = 0
  did_fit = 0
  save_left_pix = left_pix
  save_right_pix = right_pix
  niter = -1
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
              ;; --> add scaling back eventually? *10^(3.*disp_order)]
           endif
        endwhile

        ;; Make a segmented polynomial
        disp_par = pfo_fcreate(!pfo.poly, poly_order=disp_order, $
                               poly_ref=ref_pixel, poly_value=dispers, $
                               inaxis=!pfo.Xin, outaxis=!pfo.Xaxis, $
                               fop=!pfo.repl, parinfo_template=!ssg.parinfo)
        ftypes = disp_par.pfo.ftype - !pfo.poly
        prnums = round(ftypes * 100. ) ;; Reference value(s)
        pridx = where(0 lt prnums and prnums lt 10, count)
        ;; Fix the reference value(s)
        if count gt 0 then $
          disp_par[pridx].fixed = 1
        ;; Make the step sizes for the coefficient derivatives
        ;; sensible.  Coefs are 1.00n0, 1.00n1, where n is the
        ;; polynomial number.
        cftypes = ftypes * 1000.
        rcftypes = round(cftypes)
        cidx = where(0 lt rcftypes and rcftypes lt 10)
        ;; This only picks up orders 0-9
        orders = ftypes[cidx] * 1E4 - rcftypes[cidx] * 10
        sso_fmod, disp_par, cidx, step=mpstep * 10^(-(3*orders + 1))

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
           parinfo = [disp_par, deldot_par, rdot_par, lparinfo]
        endif ;; Initialize parameter list
        message, 'NOTE: set dispersion coefficients to those found in FITS header of ' + shortfile ,/INFORMATIONAL
     endif ;; Dispersion initialization
     ;; Keep dispersion handy as a separate variable.  Also the
     ;; dispersion order, though with segmented polynomials, this is
     ;; getting a bit obsolete --> assuming polynum=1
     disp_idx = where(parinfo.pfo.inaxis eq !pfo.Xin and $
                      parinfo.pfo.outaxis eq !pfo.Xaxis)
     ftypes = parinfo[disp_idx].pfo.ftype - !pfo.poly
     prnums = round(ftypes * 100. )
     pridx = where(0 lt prnums and prnums lt 10, count)
     if count ne 1 then $ $
       message, 'ERROR: ' + strtrim(count, 2) + ' reference pixels found.  I can only handle 1'
     ;; get dispersion polynomial
     cftypes =  ftypes * 1000.
     rcftypes = round(cftypes)
     c0idx = where(0 lt rcftypes and rcftypes lt 10 and $
                   round(cftypes * 10.) eq rcftypes * 10, $
                   npoly)
     if npoly ne 1 then $
       message, 'ERROR: '  + strtrim(npoly, 2) + ' dispersion polynomials found.  I can can only handle 1'
     cidx =  where(round(ftypes * 1000.) le 1)
     cidx = disp_idx[cidx]
     disp_order = N_elements(cidx)-1


     ;; NEW WAVELENGTH SCALE
     junk = pfo_funct(disp_pix, parinfo=parinfo, idx=disp_idx, $
                      xaxis=new_wavelengths)

     ;; Handle the results of any wavelength window repositioning,
     ;; including recalculating the X axis for dispersion changes and
     ;; killing off or resurrecting lines 
     if left_pix gt right_pix then begin
        temp = left_pix & left_pix = right_pix & right_pix = temp
     endif
     left_wval =  new_wavelengths[left_pix]
     right_wval = new_wavelengths[right_pix]
     temp = disp_pix
     temp[0:left_pix] = !values.f_nan
     temp[right_pix:nx-1] = !values.f_nan
     pix_axis = where(finite(temp) eq 1 and $
                      finite(wavelengths[*,0]) eq 1 and $
                      finite(spectra[*,0]) eq 1 and $
                      finite(spec_errors[*,0]) eq 1, n_pix)
     if n_pix eq 0 then begin
        message, /continue, 'WARNING: no good data found in selected range'
     endif else begin
        ;; We have good data in our selected wavelength range
        junk = pfo_funct(pix_axis, parinfo=parinfo, idx=disp_idx, xaxis=xaxis)
        spec = spectra[pix_axis, 0]
        err_spec = spec_errors[pix_axis, 0]

        ;; CONTINUUM.  It is important to do it in this order so the
        ;; median spectrum is taken from the stuff in the window, not the
        ;; whole spectrum
        cont_idx = where(parinfo.sso.ptype eq !sso.cont, N_continuum)
        if N_continuum eq 0 then begin
           message, 'NOTE: resetting continuum to the median of the displayed spectrum and, if N_continuum specified on the command line, zeroing higher order terms.  The reference point for the continuum polynomial is ' + strtrim(cont_poly_ref,2), /INFORMATIONAL
           cont_par = $
             pfo_fcreate(!pfo.sso_funct, ptype=!sso.cont, $
                         sso_ftype=!pfo.poly, poly_ref=cont_poly_ref, $
                         poly_order=N_continuum_orig-1, $
                         format=['f14.6'], eformat=['f6.2'], $
                         parinfo_template=!ssg.parinfo)
           cont_par[0].fixed = 1
           cont_par[1].value = median(spectra[*,0])
           parinfo = [parinfo, cont_par]
        endif ;; Continuum initialization
        ;; Keep continuum handy as a separate variable
        cont_idx = where(parinfo.sso.ptype eq !sso.cont, N_continuum)
        N_continuum = N_continuum - 1

        ;; Make a continuum spectrum for subtraction in graveyard
        ;; calculations.  Using original dispersion axis pixels so bad
        ;; data doesn't interfere with model calculations.
        cont_spec = pfo_funct(disp_pix[left_pix:right_pix], parinfo=parinfo, $
                              idx=[disp_idx, cont_idx])

        ;; DOPPLER SHIFTS
        sso_dg_assign, parinfo
        dop_idx = where(parinfo.sso.ptype eq !sso.dop, ndop)
        if ndop eq 0 then $
          message, 'ERROR: no Doppler shifts found'

        ;; Do a preliminary run of the model so we can recalculate the
        ;; observed wavelenghts so the graveyard works properly
        model_spec = pfo_funct(pix_axis, parinfo=parinfo)

        ;; GRAVEYARD.  

        ;; EQUIVALENT WIDTH LIMIT min_ew
        ;; Check for lines that have negligable equivalent widths and
        ;; mark them with our own status value.  In case we change
        ;; min_ew, check all the lines each time.
        ew_idx = where(parinfo.sso.ptype eq !sso.line and $
                       parinfo.sso.ttype eq !sso.ew, new)
        if new gt 0 then begin
           ;; I need to separate out the values since the structure
           ;; confuses IDL's where statement
           test_values = parinfo[ew_idx].value
           small_idx = where(abs(test_values) lt min_ew and $
                             parinfo[ew_idx].pfo.status ne !ssg.too_small, nsmall)
           if nsmall gt 0 then begin
              message, /INFORMATIONAL, 'NOTE: sending ' + strtrim(nsmall, 2) + ' lines to the graveyard because the abs value of their equiv widths are below ' + strtrim(min_ew, 2)
              for iew=0, nsmall-1 do begin
                 idx = ew_idx[small_idx[iew]]
                 dg = parinfo[idx].sso.dg ; Doppler group
                 rwl = parinfo[idx].sso.rwl ; rest wavelength
                 myidx = where(parinfo.sso.dg eq dg and $
                               parinfo.sso.rwl eq rwl)
                 ;; Mark with a special flag that is unique to the ssg
                 ;; code so we don't resurrect these lines
                 parinfo[myidx].pfo.status = !ssg.too_small
              endfor

           endif ;; found equivalent widths that are too small

           ;; Resurrect lines previously marked as too small that now
           ;; are above min_ew (i.e. min_ew changed).
           resur_idx = where(abs(test_values) ge min_ew and $
                             parinfo[ew_idx].pfo.status eq !ssg.too_small, $
                             num_resur)
           if num_resur gt 0 then begin
              message, /INFORMATIONAL, 'NOTE: resurrecting ' + strtrim(num_resur, 2) + ' lines that have equiv widths above ' + strtrim(min_ew, 2)
              for iew=0, num_resur-1 do begin
                 idx = ew_idx[resur_idx[iew]]
                 dg = parinfo[idx].sso.dg ; Doppler group
                 rwl = parinfo[idx].sso.rwl ; rest wavelength
                 myidx = where(parinfo.sso.dg eq dg and $
                               parinfo.sso.rwl eq rwl)
                 parinfo[myidx].pfo.status = !pfo.active
              endfor

          endif ;; resurrected some lines that are now not too small

        endif ;; Equivalent width testing

        ;; Find the line center indexes of things that should potentially
        ;; be removed from the active line list.  Do this in stages,
        ;; since owl has lots of NANs in it that seem to be confusing
        ;; IDL
        f_idx = where(parinfo.pfo.status eq !pfo.active, npar)
        lc_idx = where(finite(parinfo[f_idx].sso.owl) and $
                       parinfo[f_idx].sso.ttype eq !sso.center, nlc)
        num_to_grave = 0
        if nlc gt 0 then begin
           lc_idx = f_idx[lc_idx]
           grave_lc_idx $
             = where(parinfo[lc_idx].sso.owl lt left_wval or $
                     right_wval lt parinfo[lc_idx].sso.owl, $
             num_to_grave)
        endif

        if num_to_grave gt grave_report then $
          message, /INFORMATIONAL, 'NOTE: sending approx ' + strtrim(num_to_grave, 2) + ' lines outside the active region to the graveyard.'
        for il=0,num_to_grave - 1 do begin
           ;; find all the parameters for this line
           c_idx = lc_idx[grave_lc_idx[il]] ; center idx
           dg = parinfo[c_idx].sso.dg ; Doppler group
           rwl = parinfo[c_idx].sso.rwl ; rest wavelength
           if num_to_grave le grave_report then $
             message, 'NOTE: Looking at graveyard candidate ' + strjoin(sso_dg_path(dg, /name), '-') + ' ' + string(format=!sso.rwl_format, rwl), /INFORMATIONAL

           myidx = where(parinfo[f_idx].sso.dg eq dg and $
                         parinfo[f_idx].sso.rwl eq rwl)
           ;; Unwrap
           myidx = f_idx[myidx]

           ;; Now calculate the model spectrum with just this line in it
           ;; and compare that to a model spectrum with the line in the
           ;; middle of the spectrum to see if we are losing too much
           ;; area.  

           ;; Since pfo_funct recalculates owl, we need to tweak rwl
           ;; to put the line in the center of the spectrum.  For
           ;; spectra with significant non-linear dispersion, this
           ;; won't work, but that is not the case for ssg.  Do it in
           ;; this order so that owl gets restored to the proper value
           ;; when we are done here.
           dw = (left_wval + right_wval)/2. - parinfo[c_idx].sso.owl
           parinfo[myidx].sso.rwl = rwl + dw

           ;; ON AREA
           model_spec = pfo_funct(disp_pix[left_pix:right_pix], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, dop_idx, myidx])

           parinfo[myidx].sso.rwl = rwl
           model_spec = model_spec - cont_spec
           on_area = total(model_spec, /NAN)

           ;; OFF AREA
           model_spec = pfo_funct(disp_pix[left_pix:right_pix], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, dop_idx, myidx])
           model_spec = model_spec - cont_spec
           off_area = total(model_spec, /NAN)

           ;; Treat multiple lines with the same rest wavelength as one line
;           if count gt 1 or count eq 0 then begin
;              message, 'ERROR: unexpected number of lines with rwl=' + string(rwl)
;              message, 'WARNING: possible multiple entries in catalog ' + string(testparinfo[my_lc_idx[0]].ssggroupID) + ' for rest wavelength ' + string(testparinfo[my_lc_idx[0]].ssgrwl), /CONTINUE
;              testparinfo[my_lc_idx].ssgowl = !values.f_nan          
;           endif

           ;; This ratio is somewhat model and instrument profile
           ;; dependent, but I don't want to make it a command line
           ;; parameter just yet.  Split the difference between 0.5,
           ;; which is the symetric line on the edge case and something
           ;; really severe like 0.01, which would potentially be prone
           ;; to blowing up.  Also, handle the pathological case of 0
           ;; area 
           if off_area - on_area eq 0 or off_area/on_area lt 0.05 then begin
              ;; This line belongs in the graveyard.
              if num_to_grave le grave_report then $
                message, 'NOTE: Sending ' + strjoin(sso_dg_path(dg, /name), '-') + ' ' + string(format=!sso.rwl_format, rwl) + ' to the graveyard.', /INFORMATIONAL
              for iidx=0, N_elements(myidx)-1 do $
                parinfo[myidx[iidx]].pfo.status = !pfo.inactive
           endif ;; Moved a line off to the graveyard
        endfor ;; Moving lines to the graveyard

        ;; RESURRECTION

        ;; Find lines that are not too small and that are inside the
        ;; active region to see if we need to resurect them.  Bad is
        ;; not the best name but will do for now.  Make sure we avoid
        ;; lines that are too small.
        bad_idx = where(parinfo.pfo.status ne !pfo.active and $
                        parinfo.pfo.status ne !ssg.too_small, npar)
        lc_idx = where(finite(parinfo[bad_idx].sso.owl) and $
                       parinfo[bad_idx].sso.ttype eq !sso.center, nlc)
        num_resur = 0
        if nlc gt 0 then begin
           lc_idx = bad_idx[lc_idx]
           resur_lc_idx $
             = where(left_wval lt parinfo[lc_idx].sso.owl and $
                     parinfo[lc_idx].sso.owl lt right_wval, num_resur)
        endif

        if num_resur gt grave_report then $
          message, 'NOTE: Resurrecting aprox ' + strtrim(num_resur, 2) + ' lines that are now in the active region.', /INFORMATIONAL
        for il=0,num_resur - 1 do begin
           ;; find all the parameters for this line
           c_idx = lc_idx[resur_lc_idx[il]] ; center idx
           dg = parinfo[c_idx].sso.dg ; Doppler group
           rwl = parinfo[c_idx].sso.rwl ; rest wavelength
           if num_resur le grave_report then $
             message, 'NOTE: Looking at resurrection candidate ' + strjoin(sso_dg_path(dg, /name), '-') + ' ' + string(format=!sso.rwl_format, rwl), /INFORMATIONAL

           myidx = where(parinfo[bad_idx].sso.dg eq dg and $
                         parinfo[bad_idx].sso.rwl eq rwl)
           ;; Unwrap.
           myidx = bad_idx[myidx]
           ;; Don't forget to turn the parameters on so the area
           ;; calculations work!
           parinfo[myidx].pfo.status = !pfo.active

           ;; As above, calculate on and off areas
           ;; ON AREA
           dw = (left_wval + right_wval)/2. - parinfo[c_idx].sso.owl
           parinfo[myidx].sso.rwl = rwl + dw

           model_spec = pfo_funct(disp_pix[left_pix:right_pix], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, dop_idx, myidx])
           parinfo[myidx].sso.rwl = rwl

           model_spec = model_spec - cont_spec
           on_area = total(model_spec, /NAN)

           ;; OFF AREA
           model_spec = pfo_funct(disp_pix[left_pix:right_pix], $
                                  parinfo=parinfo, $
                                  idx=[disp_idx, cont_idx, dop_idx, myidx])

           model_spec = model_spec - cont_spec
           off_area = total(model_spec, /NAN)

           ;; SEE DOCUMENTATION ABOVE.  But it is a little harder to do
           ;; on the way out, since I use the window edge as the trigger
           if off_area - on_area ne 0 and off_area/on_area ge 0.5 then begin
              ;; In order to do the calculation, this line was already
              ;; resurrected.
              if num_resur le grave_report then $
                message, 'NOTE: Resurrected ' + strjoin(sso_dg_path(dg, /name), '-') + ' ' + string(format=!sso.rwl_format, rwl), /INFORMATIONAL
           endif else begin
              ;; Send line to graveyard
              for iidx=0, N_elements(myidx)-1 do $
                parinfo[myidx[iidx]].pfo.status = !pfo.inactive
           endelse
        endfor ;; resurrecting lines

        ;; LORENTZIAN WIDTH CHECK
        ;; --> this depends on the specific definition of the Voigt
        ;; function in the pfo system.
        lor_idx = where(parinfo.sso.pfo.pfo.ftype eq !pfo.voigt + 0.4 and $
                        parinfo.pfo.status eq !pfo.active, nlor)
        zero_lor_idx = !values.d_nan
        free_lor_idx = !values.d_nan
        ;; --> skip if doing initial fit of dispersion, since this
        ;; messes with fixed/free.  Kind of ugly to do it this way and
        ;; will cause problems later if lines get fixed.
        if keyword_set(idisp_fit) then $
          nlor = 0
        for ilor=0, nlor-1 do begin
           idx = lor_idx[ilor]
           dg = parinfo[idx].sso.dg ; Doppler group
           rwl = parinfo[idx].sso.rwl ; rest wavelength
           myidx = where(parinfo.sso.dg eq dg and $
                         parinfo.sso.rwl eq rwl)
           my_ew_idx = where(parinfo[myidx].sso.ttype eq !sso.ew, count)
           if count eq 0 then $
             message, 'ERROR: equivalent width parameter not found'
           ;; unnest
           my_ew_idx = myidx[my_ew_idx]
           if abs(parinfo[my_ew_idx].value) lt small_ew and $
             parinfo[idx].fixed eq 0 then begin
              zero_lor_idx = array_append(idx, zero_lor_idx)

              parinfo[idx].value = 0
              parinfo[idx].fixed = 1
           endif ;; too small
           if abs(parinfo[my_ew_idx].value) ge small_ew and $
             parinfo[idx].fixed eq 1 then begin
              free_lor_idx = array_append(idx, free_lor_idx)
;              message, /INFORMATIONAL, 'NOTE: freeing Lorentzian width of ' + strjoin(sso_dg_path(dg, /name), '-') + ' ' + string(format=!sso.rwl_format, rwl) + ' because its equivalent width is now above ' + strtrim(small_ew, 2)
              parinfo[idx].fixed = 0
           endif ;; now big enough

        endfor ;; each Lorentzian parameter

        ;; Print messages about Lorentzian width changes
        if N_elements(zero_lor_idx) gt grave_report then begin
           message, /INFORMATIONAL, 'NOTE: zeroing Lorentzian width of ' + strtrim(N_elements(zero_lor_idx),2) + ' lines because their equivalent widths are below ' + strtrim(small_ew, 2)
        endif else begin
           if finite(zero_lor_idx[0]) then begin
              for ilor=0, N_elements(zero_lor_idx)-1 do begin
                 message, /INFORMATIONAL, 'NOTE: zeroing Lorentzian width of ' + strjoin(sso_dg_path(parinfo[zero_lor_idx[ilor]].sso.dg, /name), '-') + ' ' + string(format=!sso.rwl_format, parinfo[zero_lor_idx[ilor]].sso.rwl) + ' because its equivalent width is below ' + strtrim(small_ew, 2)
              endfor
           endif
        endelse

        if N_elements(free_lor_idx) gt grave_report then begin
           message, /INFORMATIONAL, 'NOTE: freeing Lorentzian width of ' + strtrim(N_elements(free_lor_idx),2) + ' lines because their equivalent widths are above ' + strtrim(small_ew, 2)
        endif else begin
           if finite(free_lor_idx[0]) then begin
              for ilor=0, N_elements(free_lor_idx)-1 do begin
                 message, /INFORMATIONAL, 'NOTE: freeing Lorentzian width of ' + strjoin(sso_dg_path(parinfo[free_lor_idx[ilor]].sso.dg, /name), '-') + ' ' + string(format=!sso.rwl_format, parinfo[free_lor_idx[ilor]].sso.rwl) + ' because its equivalent width is above ' + strtrim(small_ew, 2)
              endfor
           endif
        endelse
        ;; End of GRAVEYARD and RESURRECTION stuff


        ;; Calculate current model and plot
        sso_dg_assign, parinfo
        model_spec = pfo_funct(pix_axis, parinfo=parinfo)

        residual = spec - model_spec
        chisq = total((residual/err_spec)^2, /NAN)
        free_idx = where(parinfo.fixed ne 1 and $
                         parinfo.pfo.status eq !pfo.active, nfree)
        dof = n_pix - nfree
        redchisq = chisq/(dof - 1)
;        ;; Make sure we have some sort of error list
;        if N_elements(perrors) ne N_elements(params) then $
;          perrors = dblarr(N_elements(params))
;        
        ;; --> I should do a limit check here just to keep mpfit happy

        wset,6

        sso_plot_fit, pix_axis, parinfo, spec, err_spec, $
                      xrange=[left_wval, right_wval], yrange=yrange, $
                      resid_yrange=resid_yrange, $
                      title=title, xtitle=xtitle, ytitle=ytitle, $
                      dop_axis_frac=dop_axis_frac

     endelse ;; no good data found in selected wavelength range


     message, /CONTINUE, 'TOP LEVEL'
     if keyword_set(idisp_fit) then $
       print, 'Initial dispersion fitting mode'
     if keyword_set(dd_fit) then $
       print, 'Dispersion and solar Doppler fitting mode'
     print, 'Minimum equiv width (milli A) = ', strtrim(min_ew, 2)
     message, /CONTINUE, 'Use left and right buttons select bracket a region of interest.  Middle button brings up menu.'

     cursor, x1, y1, /DOWN, /DATA
     ;; Left mouse
     if !MOUSE.button eq 1 then begin
        dxs = abs(new_wavelengths - x1)
        junk = min(dxs, a)
        left_pix = a[0]
     endif ;; leftmost mouse button

     ;; Right mouse
     if !MOUSE.button eq 4 then begin
        dxs = abs(new_wavelengths - x1)
        junk = min(dxs, a)
        right_pix = a[0]
     endif ;; rightmost mouse button

     ;; Middle mouse
     if !MOUSE.button eq 2 then begin
        message, /CONTINUE, 'Menu:'
        ts = 'turn on'
        if keyword_set(idisp_fit) then $
          ts = '---turn off---'
        print, ts, ' Initial dispersion fit'
        ts = 'turn on'
        if keyword_set(dd_fit) then $
          ts = '---turn off---'
        print, ts, ' Dispersion and solar Doppler fit'
        print, 'Fit '
        print, 'Print plot '
        print, 'List current parameters '
        print, 'Command prompt'
        print, 'Modify parameters'
        print, 'unZoom'
        print, 'Save in database'
        print, 'Read from database'
        print, 'Quit'
        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'I, D, [F], P, L, C, M, R, Z, S, Q?'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'F'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           answer = strupcase(answer)
        endrep until $
          answer eq 'I' or $
          answer eq 'D' or $
          answer eq 'F' or $
          answer eq 'P' or $
          answer eq 'L' or $
          answer eq 'C' or $
          answer eq 'M' or $
          answer eq 'R' or $
          answer eq 'Z' or $
          answer eq 'S' or $
          answer eq 'Q'

        if answer eq 'I' then begin
           if NOT keyword_set(idisp_fit) then begin
              disp_fit_old_min_ew = min_ew
              min_ew = modval(disp_min_ew, 'Minimum equivalent width for dispersion relation fit')

              ;; Get ready to fix everything but the dispersion relation
              idisp_fit_old_fixed = parinfo.fixed
              parinfo.fixed = 1
              ;; Free up dispersion relation...
              parinfo[disp_idx].fixed = 0
              ;; but fix the reference value(s)
              ftypes = parinfo[disp_idx].pfo.ftype - !pfo.poly
              prnums = round(ftypes * 100. )
              pridx = where(0 lt prnums and prnums lt 10, count)
              if count gt 0 then $
                parinfo[disp_idx[pridx]].fixed = 1

              idisp_fit = 1
              message, 'Reposition the plot window and do a fit.  Toggle back when you are done', /CONTINUE
           endif else begin
              min_ew = modval(disp_fit_old_min_ew, 'Minimum equivalent width for normal fitting')
              ;; Set parameters back to their previous values and fix dispersion relation 
              parinfo.fixed = idisp_fit_old_fixed
              parinfo[disp_idx].fixed = 1

              idisp_fit = 0
           endelse
        endif ;; initial dispersion relation fit

        if answer eq 'D' then begin
           if NOT keyword_set(dd_fit) then begin
              disp_fit_old_min_ew = min_ew
              min_ew = modval(disp_min_ew, 'Minimum equivalent width for dispersion and solar Doppler fitting')
              parinfo[disp_idx].fixed = 0
              ;; Fix the reference value(s)
              ftypes = parinfo[disp_idx].pfo.ftype - !pfo.poly
              prnums = round(ftypes * 100. )
              pridx = where(0 lt prnums and prnums lt 10, count)
              if count gt 0 then $
                parinfo[disp_idx[pridx]].fixed = 1

              dd_fit = 1
              message, 'Reposition the plot window and do a fit.  Toggle back when you are done', /CONTINUE
           endif else begin
              min_ew = modval(disp_fit_old_min_ew, 'Minimum equivalent width for normal fitting')
              parinfo[disp_idx].fixed = 1
              dd_fit = 0
           endelse
        endif ;; Fit dispersion and solar doppler

        if answer eq 'P' then begin
           landscape = fix(modval(landscape, 'Plot in landscape mode? (1=yes, 0=no)'))
           pfile=strmid(shortfile, 0, strpos(shortfile, '.fits')) + $
                 '_spec.ps'
           message, /CONTINUE, 'Writing postscript file ' + pfile
           set_plot,'ps'
           device, filename=pfile, landscape=landscape
           sso_plot_fit, pix_axis, parinfo, spec, err_spec, $
                         xrange=[left_wval, right_wval], yrange=yrange, $
                         resid_yrange=resid_yrange, $
                         title=title, xtitle=xtitle, ytitle=ytitle, $
                         dop_axis_frac=dop_axis_frac
           device, /close
           set_plot, 'x'
        endif ;; Print

        if answer eq 'L' then begin
           print, pfo_funct(parinfo=parinfo, idx=f_idx, print=2)
           print, '-------------------------------------------------'
           print, pfo_funct(parinfo=parinfo, idx=f_idx, print=3)
           if keyword_set(chisq) then begin
              print, niter, chisq, dof, $
                     format='("Iterations ",I6,"   chi-sq = ",G15.8,"          dof = ",I0)'
           endif
        endif ;; List parameters

        if answer eq 'C' then begin
           lc_idx = where(parinfo.sso.ttype eq !sso.center and $
                          parinfo.sso.ptype eq !sso.line, complement=non_lc_idx)
           line_idx = where(parinfo.sso.ptype eq !sso.line, $
                            complement=non_line_idx)
           message, /CONTINUE, 'disp_idx, cont_idx, line_idx, non_line_idx, lc_idx and non_lc_idx are some useful variables that have been prepared for you.  Check to make sure they stand for what you expect before you do anything (e.g. print, parinfo[disp_idx].value).  When you are done, enter .c'
           stop
        endif ;; Command prompt

        if answer eq 'M' then begin

           message, /CONTINUE, 'Modify parameters menu:'
           print, 'Minimum equivalent width'
           print, 'modify Doppler shifts'
           print, 'change dispersion Order'
           print, 'change disperSion relation'
           print, 'change Continuum order'
           print, 'Quit this menu'
;           print, 'Add lines '
;           print, 'Remove lines'
;           print, 'modify 1 Line'

           answer = ''
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'M, D, O, S, C, [Q]'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'Q'
              answer = strupcase(answer)
              for ki = 0,1000 do flush_input = get_kbrd(0)
           endrep until $
             answer eq 'A' or $
             answer eq 'M' or $
             answer eq 'D' or $
             answer eq 'O' or $
             answer eq 'S' or $
             answer eq 'C' or $
             answer eq 'Q'

           ;; Minimum equivalent width
           if answer eq 'M' then begin
              min_ew = modval(min_ew, 'Minimum equivalent width')
           endif ;; Minimum equivalent width

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
              message, 'not working yet', /CONTINUE
;              for ki = 0,1000 do flush_input = get_kbrd(0)
;              repeat begin
;                 message, /CONTINUE, 'Enter new dispersion polynomial order (9 recalculates dispersion from comp lamp value [' + string(disp_order-1) + ']'
;                 answer = get_kbrd(1)
;                 for ki = 0,1000 do flush_input = get_kbrd(0)
;              endrep until (byte(answer) ge 48 and byte(answer) le 57) $
;                or  byte(answer) eq 10
;              if byte(answer) ne 10 then disp_order = (fix(answer)+1) mod 10
;              p = 0.d
;              disp_idx = where(parinfo.ssgID eq ssgid_disp, old_disp_order)
;              ;; Add any param/parinfo entries
;              while old_disp_order lt disp_order do begin
;                 tparinfo = ssg_init_parinfo()
;                 tparinfo[0].limits = [double(p - 10.d), $
;                                       double(p + 10.d)]
;                 tparinfo[0].parname = $
;                                      string(format='("Disp. Coef ", i3)', old_disp_order)
;                 tparinfo[0].ssgID = ssgid_disp
;                 params = [params, p]
;                 parinfo = [parinfo, tparinfo]
;                 disp_idx = where(parinfo.ssgID eq ssgid_disp, old_disp_order)
;              endwhile
;              ;; Delete any param/parinfo entries
;              while old_disp_order gt disp_order do begin
;                 parinfo[disp_idx[old_disp_order-1]].ssgID = -ssgid_disp
;                 disp_idx = where(parinfo.ssgID eq ssgid_disp, old_disp_order)
;              endwhile
;              good_idx = where(parinfo.ssgID ne -ssgid_disp, count)
;              temp = params[good_idx] & params = temp
;              temp = parinfo[good_idx] & parinfo = temp

           endif ;; Change dispersion polynomial order 

           ;; Change continuum polynomial order 
           if answer eq 'C' then begin
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'Enter new number of continuum terms (polynomial order + 1) Enter 0 to reset to median of spectrum [' + string(N_continuum) + ']'
                 answer = get_kbrd(1)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until (byte(answer) ge 48 and byte(answer) le 57) $
                or  byte(answer) eq 10
              if byte(answer) ne 10 then N_continuum = fix(answer)

              ;; Find old continuum parameters
              cont_idx = where(parinfo.sso.ptype eq !sso.cont, N_continuum_orig, $
                              complement=non_cont_idx)
              N_continuum_orig = N_continuum_orig - 1
              ;; Skip time consuming array copies if we didn't change
              ;; N_continuum
              if N_continuum ne N_continuum_orig then begin
                 ;; Save cont_par and remove them from parinfo
                 cont_par = parinfo[cont_idx]
                 parinfo = parinfo[non_cont_idx]
                 ;; Use code above to reset continuum to original value
                 if N_continuum ne 0 then begin
                    new_cont_par = $
                      pfo_fcreate(!pfo.sso_funct, ptype=!sso.cont, $
                                  sso_ftype=!pfo.poly, poly_ref=cont_poly_ref, $
                                  poly_order=N_continuum-1, $
                                  format=['f14.6'], eformat=['f6.2'], $
                                  parinfo_template=!ssg.parinfo)
                    new_cont_par[0].fixed = 1
                    new_cont_par[0:min([N_continuum, N_continuum_orig])].value = $
                      cont_par[0:min([N_continuum, N_continuum_orig])].value
                    parinfo = [new_cont_par, parinfo]
                 endif ;; N_continuum is nonzero
              endif ;; changes to N_continuum

           endif ;; Change continuum polynomial order 

           ;; Modify lines
           if answer eq 'L' then begin
              message, /CONTINUE, 'Use left mouse button select line'
              cursor, x1, y1, /DOWN, /DATA
              ;; Left mouse
              if !MOUSE.button eq 1 then begin
                 ;; Find the observed line center closest to the mouse
                 ;; click + expresss it as a scaler, not an array
                 dxs = abs(parinfo.sso.owl - x1)
                 junk = min(dxs, a)
                 lc_idx = a[0]
                 ;; Now get the defining parameters for the line so
                 ;; that we can grab all of its parameters.  --> This
                 ;; will be useful in the generalized graveyard code
                 rwl = parinfo[lc_idx].sso.rwl
                 dg = parinfo[lc_idx].sso.dg
                 line_idx = where(parinfo.sso.rwl eq rwl and $
                                  parinfo.sso.dg eq sg)
;                 modpar, parinfo, line_idx
                 message, 'Sorry, you are going to have to do this by hand at the moment.  The line parameters are in line_idx.  print, parinfo[line_idx].sso.rwl to make sure it is the line you wanted.  Enter .c after you are done to continue', /continue
                 stop
              endif             ; left mouse button
           endif ;; Modify lines 'L'

           answer = ''
        endif ;; Modify parameters 'M'

        if answer eq 'F' then begin
           ;; mpfitfun is not set up to deal with overspecified
           ;; functions, so make a copy of just the active parameters
           ;; (as set in the graveyard/resurrection code)
           f_idx = where(parinfo.pfo.status eq !pfo.active, npar)
           fparinfo = parinfo[f_idx]
           functargs = {parinfo:fparinfo}
           iterargs = {Xorig:pix_axis, spec:spec, err_spec:err_spec, $
                       iterstop:1}
           params = $
             mpfitfun('pfo_funct', pix_axis, spec, err_spec, $
                      parinfo=fparinfo, functargs=functargs, $
                      autoderivative=1, ftol=ftol, xtol=xtol, $
                      resdamp=resdamp, gtol=gtol, maxiter=maxiter, $
                      quiet=quiet, nprint=nprint, iterproc=!pfo.iterproc, $ $
                      iterargs=iterargs, perror=perror, $
                      status=status, niter=niter, bestnorm=chisq)


           ;; mpfitfun is usually robust with its errors, so if we
           ;; made it here, it has something useful to say in the
           ;; status variable.  For our purposes, we either want to
           ;; keep what it did, or throw it away
           keep = 1
           case status of
              0: begin
                 fmesg = 'ERROR: internal coding error: improper input parameters to mpfit'
                 keep = 0
              end
              !pfo.iterstop: begin
                 fmesg = 'WARNING: user interrupted fit, KEEPING parameters, but errors can''t be calculated'
                 perror=!values.d_nan
              end
              !pfo.iterquit: begin
                 fmesg = 'WARNING: user interrupted fit, RESETTING parameters'
                 keep = 0
              end
              1: fmesg = 'which means chi sq has converged to better than FTOL=' + strtrim(ftol, 2)
              2: fmesg = 'which means parameters are not changing by more than XTOL' + strtrim(xtol, 2)
              3: fmesg = 'which means chi sq has converged to better than FTOL=' + strtrim(ftol, 2) + ' AND the parameters are not changing by more than XTOL' + strtrim(xtol, 2)
              4: fmesg = 'which means the abs value of the cosine of the angle between fvec and any column of the jacobian is at most GTOL=' + strtrim(gtol, 2)
              5: fmesg = 'WARNING: this means MAXITER=' + strtrim(maxiter,2) + ' was reached'
              6: fmesg = 'WARNING: this means FTOL=' + strtrim(ftol,2) + ' is too small no further reduction in the sum of squares is possible.'
              7: fmesg = 'WARNING: this means XTOL=' + strtrim(xtol,2) + ' is too small no further improvement in the approximate solution x is possible.'
              8: fmesg = 'WARNING: this means GTOL=' + strtrim(gtol,2) + ' is too small fvec is orthogonal to the columns of the jacobian to the specified precision.'
              9: message, 'ERROR: code not set up to handle external procedure'
              else: begin
                 if status le 0 then begin
                    fmesg = 'ERROR: STATUS value le 0, which looks bad.  Resetting parameters'
                    keep = 0
                 endif else begin
                    fmesg = 'NOTE: it is positive, so I am assuming it is OK'
                 endelse
              end
           endcase

           if keep then begin
              ;; Set values to fit params.  owl will be a little off,
              ;; but I wll be recalculating to display, which will fix this
              parinfo[f_idx].value = params
              parinfo[f_idx].error = perror
              did_fit = 1
              ;; Save edge pixel values to see if user moves plot window
              ;; after fit but before saving.
              save_left_pix = left_pix
              save_right_pix = right_pix
              saved = 0
           endif ;; keeping the fit

           print, pfo_funct(parinfo=parinfo, idx=f_idx, print=!pfo.pmp)
           print, niter, chisq, dof, $
                  format='("Iterations ",I6,"   chi-sq = ",G15.8,"          dof = ",I0)'
           message, /CONTINUE, 'MPFITFUN returned STATUS ' + strtrim(status,2)
           print, fmesg
        endif ;; Fit

        if answer eq 'Z' then begin
           left_pix = 0
           right_pix = nx-1
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
           ;; Save fit in an sparinfo file.  First add on markers
           ;; indicating our current wavlength endpoints in raw pixels
           ;; --> it is up to the user to save things with the active
           ;; window positioned where they did the fit.
           
           if save_left_pix ne left_pix or $
             save_right_pix ne right_pix then begin
              message, /CONTINUE, 'WARNING: you moved the plot window after you fit.  Putting it back.  PRESS SAVE AGAIN.  If you really want to hack it, get the command prompt and execute: left_pix = save_left_pix & right_pix = save_right_pix'
              left_pix = save_left_pix
              right_pix = save_right_pix
           endif else begin
              ;; Pick a semi-bogus combination of *type flags.  Has to
              ;; be active because of the way ssg_fit2ana picks out
              ;; the fit version
              end_markers = [parinfo[0], parinfo[0]]
              end_markers.fixed = 1
              end_markers.pfo.status = !pfo.active
              end_markers.pfo.ftype = 0
              end_markers.sso.ptype = !sso.line
              end_markers.sso.ttype = 0
              end_markers[0].value = left_pix
              end_markers[1].value = right_pix

              ;; Check to see if we need to open up an existing sparinfo
              ;; file
              if N_elements(sparinfo) eq 0 then begin
                 if file_test(sparinfo_fname) eq 1 then $
                   restore, sparinfo_fname, /relaxed_structure_assignment
              endif ;; Restore sparinfo file if it exists

              tparinfo = [end_markers, parinfo]

              ;; Assume we are starting a fresh sparinfo 
              tparinfo.ssg.nday = ndays[0]
              tparinfo.ssg.fver = 1
              if N_elements(sparinfo) gt 0 then begin
                 ;; check for our nday and increment fver if necessary
                 our_nday_idx = where(abs(ndays[0] - sparinfo.ssg.nday) lt 0.0001, count)
                 if count gt 0 then begin
                    fvers = sparinfo[our_nday_idx].ssg.fver
                    tparinfo.ssg.fver = max(fvers) + 1
                 endif

              endif
              sparinfo = array_append(tparinfo, sparinfo)
              save, sparinfo, filename=sparinfo_fname
              message, /CONTINUE, 'Saved version ' + strtrim(tparinfo[0].ssg.fver, 2) + ' parameters for ' +  shortfile + ',nday = ' + strtrim(nday, 2) + ' in ' + sparinfo_fname
              saved = 1

              ;; For old time sake, put some stuff into the fit db
              ;; --> this may change when I reorganize things
              oldpriv=!priv
              !priv = 2
              dbopen, fdbname, 0
              entry = where_nday_eq(ndays[0], COUNT=count,SILENT=silent, $
                                    tolerance=0.002) ; in case we were called by hand

              dbext, entry, 'fit_vers, fit_date, new_spec', $
                     fit_vers, fit_date, new_spec
              dbclose

              new_spec = 0
              get_date, today
              fit_date = today
              ;; keep the type consistent
              fit_vers = byte(tparinfo[0].ssg.fver)
              
              ;; Must do updates one file at a time
              dbopen, fdbname, 1
              dbupdate, entry, 'fit_vers, fit_date, new_spec, nfree, chisq, redchisq', fit_vers, fit_date, new_spec, fix(nfree), chisq, redchisq
              dbclose

              !priv=oldpriv
              dbclose
           endelse ;; Plot window did not move

        endif ;; Save to sparinfo file

        ;; Read from sparinfo file
        if answer eq 'R' then begin

           ;; First check to see if we just want to revert to a
           ;; previous version of this fit.
           fit_vers = 0
           rnday = ndays[0]
           if N_elements(save_sparinfo) gt 0 then begin
              save_sparinfo = temporary(sparinfo)
              our_nday_idx = $
                where(abs(ndays[0] - save_sparinfo.ssg.nday) lt 0.0001, count)
              if count gt 0 then begin
                 fvers = save_sparinfo[our_nday_idx].ssg.fver
                 fit_vers = $
                   modval(fvers, 'Fit version (0=read from another nday)')
              endif
           endif ;; We have saved parinfo, possibly for this particular spectrum
           if fit_vers eq 0 then begin
              ;; We either had no previous versions for this spectrum
              ;; or we want to read in parameters from another nday
              rnday = ssg_select([nday-0.5, nday+0.5])
              ;; beware!  in IDL 6.0, rnday is an array
              if rnday ne -1 then begin
                 ;; Find the sparinfo file for that nday
                 dbopen, rdbname, 0
                 rentry = where_nday_eq(nday, COUNT=count,SILENT=silent, tolerance=0.001)
                 if count eq 0 then message, $
                   'ERROR: THIS SHOULD NOT HAPPEN: nday ' + strtrim(rnday,2) + ' not found in ' + rdbname
                 dbext, rentry, 'dir', rdirs
                 rdirs = strtrim(rdirs, 2)
                 rsparinfo_fname = rdirs[0] + '/sparinfo_' + strtrim(round(rnday), 2) + '.sav'
                 if file_test(rsparinfo_fname) eq 1 then begin
                    message, /INFORMATIONAL, 'NOTE: reading stored parameters from ' + rsparinfo_fname
                    restore, rsparinfo_fname, /relaxed_structure_assignment
                    our_nday_idx = where(abs(rnday[0] - sparinfo.ssg.nday) lt 0.0001, count)
                    if count gt 0 then begin
                       fvers = sparinfo[our_nday_idx].ssg.fver
                       repeat begin
                          fit_vers = modval(max(fvers), 'Fit version (0=exit without changes, default=maximum)')
                       endrep until 0 le fit_vers and fit_vers le max(fvers)
                    endif

                 endif else begin
                    message, 'WARNING: no saved parameters for nday = ' + strtrim(rnday,2)
                 endelse ;; read in sparinfo from a different nday, if possible.
              endif ;; Valid rnday selected
           endif ;; User wanted to restore from a different nday

           if rnday[0] ne -1 and fit_vers ne 0 then begin
              ;; We really have a set of paramters to restore
              tidx = where(sparinfo[our_nday_idx].ssg.fver eq fit_vers, count)
              if count eq 0 then $
                message, 'ERROR: this should not happen'
              parinfo = sparinfo[our_nday_idx[tidx]]
              message, /INFORMATIONAL, 'NOTE: setting Doppler parameters to ephemeris values'
              sso_dg_assign, parinfo
              dop_idx = where(parinfo.sso.ptype eq !sso.dop, ndop)
              obj_dop_idx = where(parinfo[dop_idx].sso.dg eq obj_dg, count)
              if count gt 0 then begin
                 idx = dop_idx[obj_dop_idx]
                 value = obj_dop[0]
                 parinfo[idx].value = value
                 parinfo[idx].limits = [value - 5, value + 5]
              endif
              sun_obj_dop_idx = where(parinfo[dop_idx].sso.dg eq sun_obj_dg, $
                                      count)
              if count gt 0 then begin
                 idx = dop_idx[sun_obj_dop_idx]
                 value = sun_obj_dop[0]
                 parinfo[idx].value = value
                 parinfo[idx].limits = [value - 5, value + 5]
              endif
           endif ;; Restored parameters

           ;; Restore sparinfo if it previously existed
           if keyword_set(save_sparinfo) then $
             sparinfo = temporary(save_sparinfo)
        endif ;; Read parameters from an old fit

     endif ;; Middle mouse button

  endrep until done

end

