; $Id:

; ssg_fit1spec.pro

function io_spec, pix_X, params, dparams, parinfo=parinfo, $
                  ref_pixel=ref_pixel, spec=spec

  n_params = N_elements(params)
  npts = N_elements(pix_X)
  Y = dblarr(npts)

  nppv = 4 ;; numer of parameters per voigt
  c = 299792.458 ;; km/s

  disp_idx = where(parinfo.ssg_id eq 0, order)
  cont_idx = where(parinfo.ssg_id eq 1, N_continuum)
  solar_dop_idx = where(parinfo.ssg_id eq 2)
  io_dop_idx = where(parinfo.ssg_id eq 3)
  solar_line_idx = where(parinfo.ssg_id eq 4, n_solar)
  atm_line_idx = where(parinfo.ssg_id eq 5, n_atm)
  io_line_idx = where(parinfo.ssg_id eq 6, n_io)

  if n_params lt order+N_continuum+n_solar+n_atm+n_io then begin
     message, 'WARNING: parameters supplied do not match parinfo list',/CONTINUE
     return, Y
  endif

  n_solar = n_solar/nppv
  n_atm = n_atm/nppv
  n_io = n_io/nppv

  ;; Start with the X axis.  Since pix_X might have holes in it, this
  ;; calculates an anatomically correct wavelength at each point, no
  ;; matter how much space there is relative to its neighbors
  X = make_disp_axis(params[disp_idx], pix_X, ref_pixel)

  ;; Now put the Y axis together.  Since we are using X as the
  ;; reference now, we don't care about missing pixels
  if N_continuum gt 0 then begin
     for n=0,N_continuum-1 do begin
        Y = Y + params[cont_idx[n]]*X^n
     endfor
  endif

  ;; Now construct a parameter list for voigtfn.pro.  Start big + trim
  ;; it later.
  vps = dblarr(n_params)
  nvps = 0

  ;; Solar lines
  if n_solar gt 0 then begin
     ;; Get solar doppler shift
     dvs = 0
     if solar_dop_idx ne -1 then $
       dvs = params[solar_dop_idx]
     ;; For each line
     for isl = 0, n_solar-1 do begin
        ;; For each parameter of that line
        for ivp = 0,nppv-1 do begin
           if ivp eq 0 then begin
              ;; Adjust wavelength
              rwl = params[solar_line_idx[isl*nppv+ivp]]
              vps[nvps] = rwl * ( 1 + dvs / c )
           endif else $
             vps[nvps] = params[solar_line_idx[isl*nppv+ivp]]
           nvps = nvps + 1
        endfor
     endfor
  endif ;; Solar lines 

  ;; Atmospheric lines
  if n_atm gt 0 then begin
     ;; For each line
     for ial = 0, n_atm-1 do begin
        ;; For each parameter of that line (no doppler shift)
        for ivp = 0,nppv-1 do begin
           vps[nvps] = params[atm_line_idx[ial*nppv+ivp]]
           nvps = nvps + 1
        endfor
     endfor
  endif ;; Atmospheric lines 

  ;; Io line(s)
  if n_Io gt 0 then begin
     ;; Get Io doppler shift
     dvi = 0
     if Io_dop_idx ne -1 then $
       dvi = params[Io_dop_idx[0]]
     ;; For each line
     for iil = 0, n_Io-1 do begin
        ;; For each parameter of that line
        for ivp = 0,nppv-1 do begin
           if ivp eq 0 then begin
              ;; Adjust wavelength
              rwl = params[io_line_idx[iil*nppv+ivp]]
              vps[nvps] = rwl * ( 1 + dvi / c )
           endif else $
             vps[nvps] = params[io_line_idx[iil*nppv+ivp]]
           nvps = nvps + 1
        endfor
     endfor
  endif ;; Io lines 

  ;; Check to see if we picked up any lines
  if nvps eq 0 then return, Y

  Y = voigtfn(vps[0:nvps-1], X, Y)

  ;; Give user a chance to see plots and abort fit
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

     residual = spec - Y
     plot, X, residual, $
           title='Fit residual', xtitle=xtitle, ytitle=ytitle, $
           xstyle=1, ystyle=2
  endif
  if answer eq 'S' then message, 'STOPPING FIT'

  return, Y

end

pro ssg_fit1spec, nday, N_continuum=N_continuum, maxiter=maxiter

  if N_elements(maxiter) eq 0 then maxiter = 50

  if N_elements(nday) ne 1 then $
    message, 'ERROR: supply one and only nday at a time'

  if N_elements(N_continuum) eq 0 then N_continuum = 1

  rdbname = 'ssg_reduce'
  fdbname = 'oi_6300_fit'

  c = 299792.458 ;; km/s

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

  
  ;; This is the atmospheric line list that Melanie was using.  I wish
  ;; I could get better than this, but it is what I have for now....
  atm_lines = [6298.457d, 6299.228d, 6300.304d, 6302.000d, 6302.7642d]

  atm_lines = [atm_lines, 6299.5d, 6295.9D]

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
  order = 0
  dispers = sxpar(hdr, $
                  string(format='("DISPERS", i1)', order), count=count)
  while count ne 0 do begin
     order = order + 1
     temp = sxpar(hdr, $
                  string(format='("DISPERS", i1)', order), count=count)
     dispers = [dispers, temp]
  endwhile
  if order eq 0 then $
    message, 'ERROR: no DISPERS keyword found in header of ' + fname

  order = order - 1
  orig_dispers = dblarr(order+1)
  orig_dispers = dispers[0:order]
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

  ;; Set up the parameters that will remain with us through the whole
  ;; fitting process.  The limits section of parinfo is a bit picky,
  ;; even if you have it turned off, so lets try for realistic limits
  ;; in all cases

  ;; Dispersion.  The basis for the model
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

  ;; Solar continuum level
  m = median(orig_spec)
  continuum = [m]
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
     continuum = [continuum, 0.D]
     params = [params, 0.D]
     parinfo = [parinfo, $
                {fixed:0, $
                 limited:[0,0], $
                 limits:[-1.D,1.D], $
                 parname:'Continuum', $
                 ssg_id:1, $
                 v_id:0}]
  endfor
  old_params = params
  old_parinfo = parinfo


  done = 0
  !p.multi = [0,0,2]

  repeat begin

     if left_idx gt right_idx then begin
        temp = left_idx & left_idx = right_idx & right_idx = temp
     endif

     left_wval =  orig_wavelengths[left_idx]
     right_wval = orig_wavelengths[right_idx]

     pix_axis = orig_pix_axis[left_idx:right_idx]
     npix = N_elements(pix_axis)
     xaxis = make_disp_axis(dispers, pix_axis, ref_pixel)
     spec = orig_spec[pix_axis]
     err_axis = orig_err_axis[pix_axis]

     orig_wavelengths = make_disp_axis(dispers, orig_pix_axis, ref_pixel)
     model_spec = io_spec(pix_axis, params, $
                          parinfo=parinfo, ref_pixel=ref_pixel, spec=spec)

     ;; Extract useful things from parameter list
     cont_idx = where(parinfo.ssg_id eq 1, N_continuum)
     continuum = params[cont_idx]
     disp_idx = where(parinfo.ssg_id eq 0, count)
     if count gt 0 then dispers = params[disp_idx]
     solar_dop_idx = where(parinfo.ssg_id eq 2, count)
     if count gt 0 then dvs = params[solar_dop_idx[0]]
     io_dop_idx = where(parinfo.ssg_id eq 3, count)
     if count gt 0 then dvi = params[io_dop_idx[0]]

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
        print, 'Initialize lines in spectral range'
        print, 'Renormalize lines'
        print, 'change Doppler shifts'
        print, 'unZoom'
        print, 'Save in database'        
        print, 'Quit'
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, '[F], I, R, D, Z, Q, S?'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'F'
           answer = strupcase(answer)
        endrep until $
          answer eq 'F' or $
          answer eq 'I' or $
          answer eq 'R' or $
          answer eq 'D' or $
          answer eq 'Z' or $
          answer eq 'Q' or $
          answer eq 'S'
        for ki = 0,1000 do flush_input = get_kbrd(0)

        if answer eq 'I' then begin

           ;; Save off exising parameter stuff so we can recover values
           old_params = params
           old_parinfo = parinfo

           ;; If we have redshifted a line into our field, we need to
           ;; go back out to the blue to find it in our catalog
           left_solar_w = left_wval  * ( 1 - dvs / c )
           right_solar_w = right_wval  * ( 1 - dvs / c )
           solar_idx = where(left_solar_w le solar_atlas.lambdaD[*] and $
                             solar_atlas.lambdaD[*] le right_solar_w, $
                             n_solar)

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
           to_pass = {parinfo:parinfo, ref_pixel:ref_pixel, spec:spec}
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
