;+

; $Id:

; ssg_fit1spec.pro

; We want to build a database of atmospheric, solar, and Io emission
; lines that will be fit to the SSG spectrum.  At this high resolution
; and with boarderline signal-to-noise, it is useful to have these
; lines devided up into various groups, e.g. strong solar, stong
; atmospheric, weak solar, water vapor, etc.  As time goes on, the
; database of useful lines will grow + some weird shapes might evolve
; which are non-voigt like (e.g. saturated lines).  I will try to
; build in compatability for these things, but for now, let's start
; with Voigts.

; We want to build a structure that is usable direcly with mpfitfn,
; which will be called via some intermediate functions that take care
; of trivial calculations like Doppler shifts.  Voigtfit is one such
; function.  On that model, I should add some fields to the parinfo
; structure that will help out.  So far we have the mpfit* structure:

;     .VALUE - the starting parameter value (I use params instead)
;     .FIXED - a boolean value;  
;     .LIMITED - a two-element boolean array.
;     .LIMITS - a two-element float or double array
;     .PARNAME - a string, giving the name of the parameter.
;     .STEP - the step size for numerical derivatives.  0=autodetect
;     .MPSIDE - the sidedness of the finite difference
;     .MPMAXSTEP - the maximum change in the parameter value per iter.
;     .TIED - e.g. : parinfo(2).tied = '2 * P(1)'.
;     .MPPRINT - if set to 1, then the default ITERPROC will print

; So let's start with Voigtfit:

;	.vfID - 1=continuum parameter, 2-5 = voigt parameter
;	.ssgID -- 0 = not an SSG parameter, 1 = dispersion coef, 2 =
;                 doppler shift, 3 = continuum, 4 = Voigt
;	.ssggroupID - which group of lines this line comes from:
;			0 = not a line
;			1 = Io emission lines
;			2 = A catalogue of accurate wavelengths in the
;	                       optical spectrum of the Sun 
;	                      Allende Prieto C., Garcia Lopez R.J.
;	                     <Astron. Astrophys. Suppl. Ser. 131, 431 (1998)>
;	                     =1998A&AS..131..431A      (SIMBAD/NED BibCode)
;			3 = weak solar lines
;			4 = Strong atmospheric lines (or I might want
;			to devide these up by species)
;			5 = weak atmospheric lines
;			6 = water vapor lines
;			7 = unknown/unspecified
;		     All (4) parameters of a Voigt have this set for
;		     easy handling.
;	.ssgrwl -- rest wavelength of the line at the source
;                  (e.g. solar lines are relativistically shifted)
;	.ssgowl - observed (e.g. doppler shifted) wavelength.  The
;                 parameter actually associated with this is a
;                 wavength (or whatever) offset.  Voigtspec takes care
;                 of applying this offset before passing to voigfn.
;	.ssglink -- for emulating broad lines with multiple Voigts or
;                   for multiplets with fixed ratios.  The catalog(s)
;                   will have these in absolute form (e.g. referenced
;                   to the parameter number in that catalog), this is
;                   for the actual parameter list we have constructed.
;                   This follows the syntax of .LINK and, unlike the
;                   .ssg* parameters above, is specific to each Voigt
;                   parameter (e.g. you can link widths, etc.)  This
;                   will be painful  to implement properly when lines
;                   are removed
;	.ssgdop -- each line is assigned to a doppler group.  Lines
;                  without doppler shift (e.g. atm) have .ssgdop=0.
;                  Others are assigned to numbers by the calling
;                  program (e.g. solar = 1, Io = 2).  .ssgowl is
;                  calculated using .ssgrwl and the doppler param
;                  (ssgID = 2) with the corresponding ssgdop.
;                  For easy handling, only the linecenter shift
;                  parameter and the doppler shift parameters
;                  themselves should have .ssgdop set.

; That's about it for now.  I might want to have some other parameter
; information, like widths and accuracies, but those go into other
; parinfo places.  These addenda are mostly just handles


;-

;; AUTODERIVATIVE must be set to 1 since I don't calculate analytic
;; derivatives (a 3rd parameter)
function io_spec, pix_X, params, parinfo=parinfo, $
                  ref_pixel=ref_pixel, spec=spec, err_axis=err_axis

  ;; Make tokens for everything
  vfid_cont = 1
  vfid_center = 2
  vfid_dop = 3
  vfid_lor = 4
  vfid_area = 5
  vfid_first = vfid_center
  vfid_last = vfid_area

  ssgid_disp = 1
  ssgid_dop = 2
  ssgid_cont = 3
  ssgid_voigt = 4

  id_solar_dop = 1
  id_Io_dop = 1


  c = 299792.458 ;; km/s
  
  ;; Since pix_X might have holes in it, this calculates an
  ;; anatomically correct wavelength at each point, no matter how much
  ;; space there is relative to its neighbors
  disp_idx = where(parinfo.ssgID eq ssgid_disp, count)
  if count gt 0 then $
    X = make_disp_axis(params[disp_idx], pix_X, ref_pixel)
  else $
    X = pix_X

  ;; Doppler shifts
  dopp_idx = where(parinfo.ssgID eq ssgid_dop, n_dop)
  ;; For each doppler group find the line center parameters in that group
  for id=1,n_dop-1 do begin
     dv = params[dopp_idx[id]]
     dop_group = parinfo.ssgdop[dopp_idx[id]]
     lc_idx = where(parinfo.ssgdop eq dop_group and $
                    parinfo.ssgID ne ssgid_dop, n_lines)
     ;; For each line in that group
     for il = 0,n_lines-1 do begin
        parinfo.ssgowl[lc_idx[il]] = $
          parinfo.ssgrwl[lc_idx[il]] * ( 1. + dv / c )
     endfor
     ;; I am not going to tweak lc (line center) parameters since they
     ;; are one of the things that is being fit
  endfor
  

  ;; voigspec is not as delicate when it comes to its parameter list
  ;; as Voigfn, so I can just pass all the parameters right on through

  Y = voigtspec(X, params, parinfo=parinfo, Y=Y)

  ;; Give user a chance to see plots and abort fit.  Craig might have
  ;; a better way of doing this, but this is what I have for now.
  answer = strupcase(get_kbrd(0))
  if answer eq 'D' then begin
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

     wset,7
     plot, X, spec, psym=dot, title='Intermediate Result', $
           xtitle=xtitle, ytitle=ytitle, $
           xrange=[min(X), max(X)], yrange=[min(spec), max(spec)], $
           xstyle=1, ystyle=2
     oplot, X, Y, linestyle=dotted
     oploterr, xaxis, spec, err_axis, dot
     residual = spec - Y
     plot, X, residual, $
           title='Fit residual', xtitle=xtitle, ytitle=ytitle, $
           xstyle=1, ystyle=2
  endif
  if answer eq 'S' then message, 'STOPPING FIT'

  return, Y

end



pro ssg_fit1spec, nday, N_continuum=N_continuum_orig, maxiter=maxiter

  if N_elements(maxiter) eq 0 then maxiter = 50

  if N_elements(nday) ne 1 then $
    message, 'ERROR: supply one and only nday at a time'

  if N_elements(N_continuum_orig) eq 0 then N_continuum_orig = 1

  rdbname = 'ssg_reduce'
  fdbname = 'oi_6300_fit'

  ;; Make tokens for everything
  vfid_cont = 1
  vfid_center = 2
  vfid_dop = 3
  vfid_lor = 4
  vfid_area = 5
  vfid_first = vfid_center
  vfid_last = vfid_area

  ssgid_disp = 1
  ssgid_dop = 2
  ssgid_cont = 3
  ssgid_voigt = 4

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

  c = 299792.458 ;; km/s

  message, /INFO, 'fitting spectrum nday = ' + string(nday)

  solar_template= {   VERSION         :	     1.00000		, $
                      DATASTART       :		  0 		, $
                      DELIMITER       :	  ''	    		, $
                      MISSINGVALUE    :		 !values.f_nan	, $
                      COMMENTSYMBOL   :   ''			, $
                      FIELDCOUNT      :		  7		, $
                      FIELDTYPES      :   [5,5,5,5,7,4,4]	, $
                      FIELDNAMES      :   ['lambdaD', 'e_lambdaD', 'lambdaF', 'e_lambdaF', 'Ion', 'EP', 'loggf'], $
                      FIELDLOCATIONS  :   [0,10,17,27,34,38,43]	, $
                      FIELDGROUPS     :   [0,1,2,3,4,5,6]}
  
  solar_atlas=read_ascii('/home/jpmorgen/data/solar_atlas/table1.dat', $
                         template=solar_template)

  weak_solar_lines = [6298.9700d, 6299.5962d, 6299.5962d, 6301.0d, 6301.86d]

  
  ;; This is the atmospheric line list that Melanie was using.  I wish
  ;; I could get better than this, but it is what I have for now....
  atm_lines = [6298.457d, 6299.228d, 6300.304d, 6302.000d, 6302.7642d]

  weak_atm_lines = [6299.5d, 6295.9D]

  io_lines = [6300.304d]        ; I want to eventually put the Na in here

  dbopen, rdbname, 0
  rentry = where_nday_eq(nday, COUNT=count,SILENT=silent, tolerance=0.001)
  if count eq 0 then message, $
    'ERROR: nday ' + string(nday) + ' not found in ' + rdbname

  dbext, rentry, 'nday, dir, fname, object, date, bad, wavelen, spectrum, spec_err, cross_disp, cross_err', ndays, dirs, files, objects, dates, badarray, wavelengths, spectra, spec_errors, cross_disps, cross_errors

  dirs = strtrim(dirs)
  files=strtrim(files)
  objects=strtrim(objects)
  shortfile= strmid(files[0], $
                    strpos(files[0], '/', /REVERSE_SEARCH) + 1)
  dbclose

  ;; Try to get stuff from Melanie's catalog.  Her ndays are 3.99536 hours
  ;; behind my ndays for some reason
  dbopen,'io6300_integrated',0  
  io6300_entry = where_nday_eq(nday-(3.99536/24.), COUNT=count,SILENT=silent, tolerance=0.002) 
  if count eq 0 then begin
     message, /continue, 'WARNING: unable to find database entry.  Doppler shifts will start at 0.'
     dvs = 0.D
     dvi = 0.D
  endif else begin ; found entry in Melanie's database
     dbext, io6300_entry, 'deldot, rdot', deldots, rdots
     dvi = double(deldots[0])
     dvs = dvi + double(rdots[0])
  endelse ; found entry in Melanie's database
  dbclose  

  cd, dirs[0]
  im=ssgread(files[0], hdr)
  asize = size(im) & nx = asize(1) & ny = asize(2)

  ;; Read in dispersion coefficients so I can use that as part of the
  ;; fit
  disp_order = 0
  dispers = sxpar(hdr, $
                  string(format='("DISPERS", i1)', disp_order), count=count)
  while count ne 0 do begin
     disp_order = disp_order + 1
     temp = sxpar(hdr, $
                  string(format='("DISPERS", i1)', disp_order), count=count)
     dispers = [dispers, temp]
  endwhile
  if disp_order le 1 then $
    message, 'ERROR: not enough DISPERS keyword found in header of ' + fname

  orig_dispers = dblarr(disp_order)
  orig_dispers = dispers[0:disp_order-1]
  dispers = orig_dispers

  window,6
  window,7
  title=string(objects[0], ' ', shortfile, ' ', dates[0])
  ;; This has to be in font !3 for the angstrom symbol to be found
  xtitle='Wavelength ('+string("305B)+')' ;"
  ytitle=string('Signal (', sxpar(hdr, 'BUNIT'), '/S)')

  ;; This is the original pix_axis, on which the wavelength solution
  ;; is built.  By using make_disp_axis, it is OK if we poke holes in
  ;; this

  orig_pix_axis = where(finite(wavelengths[*,0]) eq 1 and $
                        finite(spectra[*,0]) eq 1 and $
                        finite(spec_errors[*,0]) eq 1, n_pix)
  if n_pix eq 0 then begin
     message, /continue, 'ERROR: no good data found in ' + files[0]
     return
  endif

  pix_axis = orig_pix_axis
  orig_wavelengths = wavelengths[pix_axis]
  orig_spec = spectra[pix_axis]
  orig_err_axis = spec_errors[pix_axis]

  model_spec = fltarr(n_pix)

  left_wval = min(orig_wavelengths, a)
  left_idx = a[0]
  right_wval = max(orig_wavelengths, a)
  right_idx = a[0]

  pix_axis = orig_pix_axis
  ref_pixel = nx/2.

  ;; Need some initial starting structure or else IDL complains
  parinfo = ssg_init_parinfo(1)

  done = 0
  !p.multi = [0,0,2]

  repeat begin
     
     ;; Extract useful things from parameter list, recording any
     ;; changes since the last iteration.  Also check to see if this
     ;; is the fuirst time around or if we have deleted parameters in
     ;; order for them to be reinitialized.
     
     ;; DISPERSION
     disp_idx = where(parinfo.ssg_id eq ssgid_disp, disp_order)
     ;; Check for initilization of params/parinfo
     if disp_order le 1 then begin
        ;; We need an initization or reset.  Do it in a temporary
        ;; structure + append or set up for the first time the real
        ;; params/parinfo structure from that
        disp_order = N_elements(orig_dispers)
        tparams = orig_dispers
        tparinfo = ssg_init_parinfo(disp_order)
        for idsp = 0,disp_order-1 do begin
           p = tparams[idsp] 
           ;; Kind of bogus limits, but thes will keep the mpfit code happy
           tparinfo.limits[idsp] = [p - 0.01, p + 0.01]
           tparinfo.parname[idsp]=string(format='("Disp Coef ", i3)', idsp)
           tparinfo.ssgID[idsp]=ssgid_disp
        endfor
        ;; check to see if we have any parameters
        if N_elements(params) le 1 then begin
           message, 'NOTE: initilizaing parameter list' ,/INFORMATIONAL
           params = tparams
           parinfo= tparinfo
        endif ;; Initilize parameter list
        message, 'NOTE: set dispersion coefficients to those found in FITS header of ' + fname ,/INFORMATIONAL
     endif ;; Dispersion initialization
     ;; Keep dispersion handy as a separate variable
     disp_idx = where(parinfo.ssg_id eq ssgid_disp, disp_order)
     dispers = params[disp_idx]

     ;; Handle the results of any wavelength window repositioning,
     ;; including recalculating the X axis for dispersion changes
     if left_idx gt right_idx then begin
        temp = left_idx & left_idx = right_idx & right_idx = temp
     endif
     left_wval =  orig_wavelengths[left_idx]
     right_wval = orig_wavelengths[right_idx]
     pix_axis = orig_pix_axis[left_idx:right_idx]
     xaxis = make_disp_axis(dispers, pix_axis, ref_pixel)
     spec = orig_spec[pix_axis]
     err_axis = orig_err_axis[pix_axis]
     orig_wavelengths = make_disp_axis(dispers, orig_pix_axis, ref_pixel)
     
     ;; CONTINUUM.  It is important to do it in this order so the
     ;; median spectrum is taken from the stuff in the window, not the
     ;; whole spectrum
     cont_idx = where(parinfo.ssg_id eq ssgid_cont, N_continuum)
     if N_continuum eq 0 then begin
        message, 'NOTE: resetting continuum to median of displayed spectrum and, if N_continuum specified on the command line, zeroing higher order terms ', /INFORMATIONAL
        m = median(orig_spec)
        tparams = dblarr(N_continuum_orig)
        tparams[0] = m
        tparinfo = ssg_init_parinfo(N_continuum_orig)
        for ic = 0,N_continuum-1 do begin
           p = tparams[idx] 
           ;; --> I have no idea if these are reasonable absolute boundardies
           tparinfo.limits[ic] = [p - 1., p + 1.]
           tparinfo.parname[ic]=string(format='("Cont. Poly Coef ", i3)', ic)
           tparinfo.ssgID[ic]=ssgid_cont
        endfor
        ;; Limits on 0th order should be broad they can be constrained
        ;; by the used later.  --> NOTE: this assumes a positive
        ;; continuum + could be improved if necessary, but I don't
        ;; want to use min/max(spec) unless I multiply it by something
        ;; or add the median, since the true continuum might be really
        ;; high or low....
        tparinfo.limits[0] = [-m, 2.D*m]
        params = [params, tparams]
        parinfo = [parinfo, tparinfo]
     endif ;; Continuum initialization
     ;; Keep continuum handy as a separate variable
     cont_idx = where(parinfo.ssg_id eq ssgid_disp, N_continuum)
     continuum = params[cont_idx]

     ;; DOPPLER SHIFTS.  None are necessary since atmospheric lines
     ;; have ssgdop = 0.
     dopp_idx = where(parinfo.ssg_id eq ssgid_dop, N_dopp)

     model_spec = io_spec(pix_axis, params, parinfo=parinfo, $
                          ref_pixel=ref_pixel, spec=spec, err_axis=err_axis)
     
     residual = spec - model_spec
     
     wset,6
     plot, xaxis, spec, psym=dot, title=title, xtitle=xtitle, ytitle=ytitle, $
           xrange=[min(xaxis), max(xaxis)], yrange=[min(spec), max(spec)], $
           xstyle=1, ystyle=2
     oploterr, xaxis, spec, err_axis, dot
     oplot, xaxis, model_spec, linestyle=dotted

     plot, xaxis, residual, $
           title='Fit residual', xtitle=xtitle, ytitle=ytitle, $
           xstyle=1, ystyle=2

     message, /CONTINUE, 'Use left and right buttons select bracket a region of interest.  Middle button brings up menu.'

     cursor, x1, y1, /DOWN, /DATA
     if !MOUSE.button eq 1 then begin
        dxs = abs(orig_wavelengths - x1)
        junk = min(dxs, a)
        left_idx = a[0]
     endif ;; leftmost mouse button

     if !MOUSE.button eq 2 then begin
        message, /CONTINUE, 'Menu:'
        print, 'Fit '
        print, 'Modify parameters'
        print, 'Renormalize lines'
        print, 'unZoom'
        print, 'Save in database'        
        print, 'Quit'
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, '[F], M, R, Z, S, Q?'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'F'
           answer = strupcase(answer)
        endrep until $
          answer eq 'F' or $
          answer eq 'M' or $
          answer eq 'R' or $
          answer eq 'Z' or $
          answer eq 'S' or $
          answer eq 'Q'
        for ki = 0,1000 do flush_input = get_kbrd(0)

        if answer eq 'M' then begin
           message, /CONTINUE, 'Menu:'
           print, 'Add lines '
           print, 'Remove lines'
           print, 'Modify 1 line'
           print, 'modify Doppler shifts'
           print, 'Change dispersion relation'
           print, 'Quit this menu'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'A, R, M, D, C, [Q]'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'Q'
              answer = strupcase(answer)
           endrep until $
             answer eq 'A' or $
             answer eq 'R' or $
             answer eq 'M' or $
             answer eq 'D' or $
             answer eq 'C' or $
             answer eq 'Q'
           for ki = 0,1000 do flush_input = get_kbrd(0)

           ;; Save off exising parameter stuff so we can recover values
           old_params = params
           old_parinfo = parinfo

           if answer eq 'A' then begin
              message, /CONTINUE, 'Line lists (only S and A are case sensitive):'
              print, 'Strong solar lines (S) '
              print, 'Weak solar lines (s)'
              print, 'strong atmospheric lines (A)'
              print, 'weak atmospheric lines (a)'
              print, 'Io line'
              print, 'Random line by hand'
              print, 'Quit this menu (Q or q)'
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'S, s, A, a, I, R, [Q]'
                 answer = get_kbrd(1)
                 if byte(answer) eq 10 then answer = 'Q'
                 uanswer = strupcase(answer)
              endrep until $
                uanswer eq 'S' or $
                uanswer eq 'A' or $
                uanswer eq 'I' or $
                uanswer eq 'R' or $
                uanswer eq 'Q'
              for ki = 0,1000 do flush_input = get_kbrd(0)
              
              if uanswer eq 'S' then begin
                 temp = string(dvs)
                 read, 'Solar doppler shift [' + temp + '] ', temp
                 if strlen(temp) gt 0 then dvs = double(temp)
                 ;; Check for initialization
                 sd_idx = where(parinfo.ssgID eq ssgid_dop $
                                and parinfo.ssgdop eq id_solar_dop, count)
                 if count eq 0 then begin
                    tparinfo = ssg_init_parinfo(1)
                    ;; --> Again, a wild guess, assuming we are in
                    ;; km/s, etc.
                    tparinfo.limits = [dvs-100,dvs+100]
                    tparinfo.parname = 'Solar Doppler Shift'
                    tparinfo.ssgID = ssgid_dop
                    tparinfo.ssgdop = id_solar_dop
                    params = [params, dvs]
                    parinfo = [parinfo, tparinfo]
                 endif
                 ;; Get more specific with limits, etc., using
                 ;; parameter modification 
                 sd_idx = where(parinfo.ssgID eq ssgid_dop $
                                and parinfo.ssgdop eq id_solar_dop)
                 params[sd_idx] = dvs

                 ;; If we have redshifted a line into our field, we need to
                 ;; go back out to the blue to find it in our catalog
                 left_solar_w = left_wval  * ( 1 - dvs / c )
                 right_solar_w = right_wval  * ( 1 - dvs / c )

              endif ;; adding some solar lines, strong or weak

              if uanswer eq 'I' then begin
                 temp = string(dvi)
                 read, 'Io doppler shift [' + temp + '] ', temp
                 if strlen(temp) gt 0 then dvi = double(temp)
                 ;; Check for initialization
                 iod_idx = where(parinfo.ssgID eq ssgid_dop $
                                 and parinfo.ssgdop eq id_Io_dop, count)
                 if count eq 0 then begin
                    tparinfo = ssg_init_parinfo(1)
                    ;; --> Again, a wild guess, assuming we are in
                    ;; km/s, etc.
                    tparinfo.limits = [dvi-100,dvi+100]
                    tparinfo.parname = 'Io Doppler Shift'
                    tparinfo.ssgID = ssgid_dop
                    tparinfo.ssgdop = id_Io_dop
                    params = [params, dvi]
                    parinfo = [parinfo, tparinfo]
                 endif
                 ;; Get more specific with limits, etc., using
                 ;; parameter modification 
                 iod_idx = where(parinfo.ssgID eq ssgid_dop $
                                 and parinfo.ssgdop eq id_solar_dop)
                 params[iod_idx] = dvi

                 ;; If we have redshifted a line into our field, we need to
                 ;; go back out to the blue to find it in our catalog
                 left_Io_w = left_wval  * ( 1 - dvi / c )
                 right_Io_w = right_wval  * ( 1 - dvi / c )

              endif ;; adding Io line

              ;; Strong solar lines
              if answer eq 'S' then begin
                 solar_idx = where(left_solar_w le solar_atlas.lambdaD[*] and $
                                   solar_atlas.lambdaD[*] le right_solar_w, $
                                   n_solar)
                 ;; For each line
                 for isl = 0, n_solar-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = solar_atlas.lambdaD[solar_idx[isl]]
                    old_idx = where(old_parinfo.ssggroupID eq 2 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       ew = solar_atlas.e_lambdaD[solar_idx[isl]]
                       ;; Take a wild guess at the initial parameters
                       tparinfo = ssg_init_parinfo(voigt=[wl, 0.5, 0., -exp(solar_atlas.loggf[solar_idx[isl]]) * continuum[0]/3.]
                       tparinfo.limited[0] = [1,1]
                       tparinfo.limits[0] = [-ew, ew]


                    ;; We allow these to jitter a little, so check for
                    ;; something that won't change.
                 if count eq 0 then begin
                    ;; First initialization.  Widths are in Angstroms
                    wl = wl & dw = 0.5 & dl = 0.1
                    ;; A wild guess
                    area = 
                 endif else begin
                    ;; Transfer over the old parameters
                    id = old_idx[0]
                    wl = old_params[id]
                    dw = old_params[id+1]
                    dl = old_params[id+2]
                    area = old_params[id+3]
                 endelse
                 ;; Now dump these into the params and parinfo structure
                 params = [params, wl]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:wlimits, $
                             parname:'Solar line ' + string(isl) + ' wavelength', $
                             ssg_id:4, $
                             v_id:1}]

                 ;; Doppler Width.  
                 params = [params, dw] 
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:[0.2d,0.75d], $
                             parname:'Solar line ' + string(isl) + ' Doppler width', $
                             ssg_id:4, $
                             v_id:2}]

                 ;; Lorenzian Width.
                 params = [params, dl]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:[0d,1d], $
                             parname:'Solar line ' + string(isl) + ' Lorenzian width', $
                             ssg_id:4, $
                             v_id:3}]

                 ;; Area.  Constrain to be negative.
                 params = [params, area]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[0,1], $
                             limits:[-10.d*area,0d], $
                             parname:'Solar line ' + string(isl) + ' Area', $
                             ssg_id:4, $
                             v_id:4}]

              endfor ;; Solar line loop


           endif ;; Adding lines


           ;; These of course don't shift
           atm_idx = where(left_wval le atm_lines and $
                           atm_lines le right_wval, n_atm)

           left_io_w = left_wval  * ( 1 - dvi / c )
           right_io_w = right_wval  * ( 1 - dvi / c )
           io_idx = where(left_io_w le io_lines and $
                          io_lines le right_io_w, n_io)

           ;; This is initialized above, but I might have increased
           ;; the order
           params = dispers[0]
           limits = dispers[0] + dispers[0]*[-0.0001, 0.0001]
           parinfo = {fixed:0, $
                      limited:[0,0], $
                      limits:limits, $
                      parname:'Wavelength reference', $
                      ssg_id:0, $
                      v_id:0}
           for id = 1, order do begin
              d = dispers[id]
              params = [params, d]
              limits = [d-1., d+1.]
              parinfo = [parinfo, $
                         {fixed:0, $
                          limited:[0,0], $
                          limits:limits, $
                          parname:'Dispersion order ' + string(id), $
                          ssg_id:0, $
                          v_id:0}]
           endfor

           ;; Continuum
           m = continuum[0]
           params = [params, m]
           limits = [-1.D*m, 2.D*m]
           parinfo = [parinfo, $
                      {fixed:0, $
                       limited:[0,0], $
                       limits:limits, $
                       parname:'Continuum', $
                       ssg_id:1, $
                       v_id:0}]

           ;; Hopefully we won't need much variation on these.  This
           ;; just makes sure mpfitfn doesn't complain when it first initializes
           for ic = 1,N_continuum-1 do begin
              params = [params, continuum[ic]]
              parinfo = [parinfo, $
                         {fixed:0, $
                          limited:[0,0], $
                          limits:[-1.D,1.D], $
                          parname:'Continuum', $
                          ssg_id:1, $
                          v_id:0}]
           endfor
           

           ;; Solar 
           if n_solar gt 0 then begin
              ;; Doppler shift
              params = [params, dvs]
              parinfo = [parinfo, {fixed:0, $
                                   limited:[1,1], $
                                   limits:[-100.D,100.D], $
                                   parname:'Solar Doppler Shift', $
                                   ssg_id:2, $
                                   v_id:0}]
              ;; Lines
              for isl = 0, n_solar-1 do begin
                 ;; Wavelength
                 wl = solar_atlas.lambdaD[solar_idx[isl]]
                 ew = solar_atlas.e_lambdaD[solar_idx[isl]]
                 wlimits = [wl - 5*ew, wl + 5*ew]
                 ;; We allow these to jitter a little, so check for
                 ;; something that won't change.
                 old_idx = where(old_parinfo.ssg_id eq 4 and $
                                 5*old_parinfo.limits[0] eq ew[0], count)
                 if count eq 0 then begin
                    ;; First initialization.  Widths are in Angstroms
                    wl = wl & dw = 0.5 & dl = 0.1
                    ;; A wild guess
                    area = -exp(solar_atlas.loggf[solar_idx[isl]]) * $
                           continuum[0]/3.
                 endif else begin
                    ;; Transfer over the old parameters
                    id = old_idx[0]
                    wl = old_params[id]
                    dw = old_params[id+1]
                    dl = old_params[id+2]
                    area = old_params[id+3]
                 endelse
                 ;; Now dump these into the params and parinfo structure
                 params = [params, wl]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:wlimits, $
                             parname:'Solar line ' + string(isl) + ' wavelength', $
                             ssg_id:4, $
                             v_id:1}]

                 ;; Doppler Width.  
                 params = [params, dw] 
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:[0.2d,0.75d], $
                             parname:'Solar line ' + string(isl) + ' Doppler width', $
                             ssg_id:4, $
                             v_id:2}]

                 ;; Lorenzian Width.
                 params = [params, dl]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:[0d,1d], $
                             parname:'Solar line ' + string(isl) + ' Lorenzian width', $
                             ssg_id:4, $
                             v_id:3}]

                 ;; Area.  Constrain to be negative.
                 params = [params, area]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[0,1], $
                             limits:[-10.d*area,0d], $
                             parname:'Solar line ' + string(isl) + ' Area', $
                             ssg_id:4, $
                             v_id:4}]

              endfor ;; Solar line loop
           endif ;; Any solar lines

           ;; Io
           if n_io gt 0 then begin
              ;; Doppler shift
              params = [params, dvi]
              parinfo = [parinfo, {fixed:0, $
                                   limited:[1,1], $
                                   limits:[-100.D,100.D], $
                                   parname:'Io Doppler Shift', $
                                   ssg_id:3, $
                                   v_id:0}]
              ;; Lines
              for iil = 0, n_io-1 do begin
                 ;; Wavelength
                 wl = io_lines[io_idx[iil]]
                 old_idx = where(old_parinfo.ssg_id eq 6 and $
                                 old_params eq wl, count)
                 if count eq 0 then begin
                    ;; First initialization.  Widths are in Angstroms
                    wl = wl & dw = 0.08 & dl = 0.
                    ;; A wild guess
                    area = 1.
                 endif else begin
                    ;; Transfer over the old parameters
                    id = old_idx[0]
                    wl = old_params[id]
                    dw = old_params[id+1]
                    dl = old_params[id+2]
                    area = old_params[id+3]
                 endelse

                 ;; Now dump these into the params and parinfo structure
                 params = [params, wl]
                 wlimits = [wl - 0.001, wl + 0.001]
                 parinfo = [parinfo, $
                            {fixed:1, $
                             limited:[0,0], $
                             limits:wlimits, $
                             parname:'Io line ' + string(iil) + ' wavelength', $
                             ssg_id:6, $
                             v_id:1}]

                 ;; Doppler Width
                 params = [params, dw]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:[.01d,.2d], $
                             parname:'Io line ' + string(iil) + ' Doppler width', $
                             ssg_id:6, $
                             v_id:2}]

                 ;; Lorenzian Width
                 params = [params, dl]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,1], $
                             limits:[0d,.5d], $
                             parname:'Io line ' + string(iil) + ' Lorenzian width', $
                             ssg_id:6, $
                             v_id:3}]

                 ;; Area.  Constrain to be positive
                 params = [params, area]
                 parinfo = [parinfo, $
                            {fixed:0, $
                             limited:[1,0], $
                             limits:[0d,10d], $
                             parname:'Io line ' + string(iil) + ' Area', $
                             ssg_id:6, $
                             v_id:4}]

              endfor ;; Io line loop

           endif ;; Any Io lines 

           ;; Atmospheric Lines
           for ial = 0, n_atm-1 do begin
              ;; Wavelength
              wl = atm_lines[atm_idx[ial]]
              old_idx = where(old_parinfo.ssg_id eq 5 and $
                              old_params eq wl, count)
              if count eq 0 then begin
                 wl = wl & dw = 0.2 & dl = 0.
                 ;; Start lines out near zero so they can be
                 ;; absorption or emission
                 area = -0.01
                 ;; Except for this one, which is strong
                 if wl eq 6299.228d then area = -5.
              endif else begin
                 id = old_idx[0]
                 wl = old_params[id]
                 dw = old_params[id+1]
                 dl = old_params[id+2]
                 area = old_params[id+3]
              endelse
              params = [params, wl]
              wlimits = [wl - 0.001, wl + 0.001]
              parinfo = [parinfo, $
                         {fixed:1, $
                          limited:[0,0], $
                          limits:wlimits, $
                          parname:'Atm line ' + string(ial) + ' wavelength', $
                          ssg_id:5, $
                          v_id:1}]

              ;; Doppler Width
              params = [params, dw]
              parinfo = [parinfo, $
                         {fixed:0, $
                          limited:[1,1], $
                          limits:[.01d,.5d], $
                          parname:'Atm line ' + string(ial) + ' Doppler width', $
                          ssg_id:5, $
                          v_id:2}]

              ;; Lorenzian Width
              params = [params, dl]
              parinfo = [parinfo, $
                         {fixed:0, $
                          limited:[1,1], $
                          limits:[0.d,0.5d], $
                          parname:'Atm line ' + string(ial) + ' Lorenzian width', $
                          ssg_id:5, $
                          v_id:3}]

              ;; Area.  No constraints, some may be positive
              params = [params, area]
              parinfo = [parinfo, $
                         {fixed:0, $
                          limited:[0,0], $
                          limits:[-100d,100d], $
                          parname:'Atm line ' + string(ial) + ' Area', $
                          ssg_id:5, $
                          v_id:4}]

              endfor ;; Atm line loop


           for ip = 0, N_elements(params)-1 do begin
              print, parinfo[ip], params[ip]
           endfor

        endif ;; re Initialize parameters

        ;; Change Doppler shifts
        if answer eq 'D' then begin

           for ip = 0, N_elements(params)-1 do begin
              print, parinfo[ip], params[ip]
           endfor

           temp = string(dispers[0])
           read, 'Reference Wavelength [' + temp + ']', temp
           if strlen(temp) gt 0 then dispers[0] = double(temp)

           temp = string(dvs)
           read, 'Solar doppler shift [' + temp + ']', temp
           if strlen(temp) gt 0 then dvs = double(temp)

           temp = string(dvi)
           read, 'Io doppler shift [' + temp + ']', temp
           if strlen(temp) gt 0 then dvi = double(temp)

           disp_idx = where(parinfo.ssg_id eq 0, count)
           if count gt 0 then params[disp_idx[0]] = dispers[0]
           solar_dop_idx = where(parinfo.ssg_id eq 2, count)
           if count gt 0 then params[solar_dop_idx[0]] = dvs
           io_dop_idx = where(parinfo.ssg_id eq 3, count)
           if count gt 0 then params[io_dop_idx[0]] = dvi

        endif ;; Change Doppler shifts

        if answer eq 'F' then begin
           old_params = params
           old_parinfo = parinfo
           to_pass = {parinfo:parinfo, ref_pixel:ref_pixel, spec:spec, $
                      err_spec=err_spec}
           params = mpfitfun('io_spec', pix_axis, spec, err_axis, $
                             params, FUNCTARGS=to_pass, AUTODERIVATIVE=1, $
                             PARINFO=parinfo, maxiter=maxiter)

           ;; recover from error condition
           if N_elements(params) ne N_elements(old_params) then $
             params = old_params

        endif ;; Fit

        ;; Renormalize
        if answer eq 'R' then begin
           ;; I can't seem to get this code to work: 
;% MPFIT: Error detected while in the termination phase:
;% MPFIT: Variable is undefined: XNEW.
;% MPFIT: Error condition detected. Returning to MAIN level.
;% MPFITFUN: ERROR: no free parameters

           old_params = params
           old_parinfo = parinfo
           fix_idx = where(parinfo.v_id eq 1 or count)
           if count ne 0 then parinfo.fixed[fix_idx] = 1
           to_pass = {parinfo:parinfo, ref_pixel:ref_pixel, spec:spec}
           params = mpfitfun('io_spec', pix_axis, spec, err_axis, $
                             params, FUNCTARGS=to_pass, AUTODERIVATIVE=1, $
                             PARINFO=parinfo, maxiter=maxiter)

           if N_elements(params) ne N_elements(old_params) then $
             params = old_params

           parinfo = old_parinfo

           ;; SO let's try a more old-fashioned approach

        endif ;; Renormalize

        if answer eq 'Z' then begin
           left_idx = min(orig_pix_axis)
           right_idx = max(orig_pix_axis)
        endif
        if answer eq 'Q' then begin
           done = 1
        endif
        if answer eq 'S' then begin
           print, 'open database, etc.'
        endif

     endif ;; Middle mouse button



     if !MOUSE.button eq 4 then begin
        dxs = abs(orig_wavelengths - x1)
        junk = min(dxs, a)
        right_idx = a[0]
     endif ;; rightmost mouse button


  endrep until done

  !p.multi = 0

  
end
