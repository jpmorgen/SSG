;+

; $Id: ssg_fit1spec.pro,v 1.5 2002/12/16 13:37:43 jpmorgen Exp $

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
;                 doppler shift, 3 = continuum, 4 = Voigt. 
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
;			5 = atmospheric emission lines
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

;  use -1 in any of the ID fields as a flag for deletion


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

pro ssg_fit1spec, nday, N_continuum=N_continuum_orig, maxiter=maxiter

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

  id_no_dop = 0
  id_solar_dop = 1
  id_Io_dop = 2
  doppler_names = ['none/atm', 'Solar', 'Io']
  dopplers = [0d, 0d, 0d]
  group_names = ['Not a line', 'IO LINE', 'sol cat', 'sol sup', 'atm abs', 'atm emi']
  vpnames = ['Dlambda', 'DopFWHM', 'LorFWHM', 'Area']

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
  
  ;; Do I need to convert these to air wavelengths?  Wavelenghts are
  ;; 0.01 A higher than the ones Melanie was using.  --> Use  Edlin's
  ;; formula, but for now, 
  solar_atlas=read_ascii('/home/jpmorgen/data/solar_atlas/table1.dat', $
                         template=solar_template)

  solar_atlas.lambdaD = solar_atlas.lambdaD - 0.01
  weak_solar_lines = [6298.97d, 6299.5962d, 6301.0d, 6301.86d]

  
  ;; This is the atmospheric line list that Melanie was using.  I wish
  ;; I could get better than this, but it is what I have for now....
  atm_absorb = [6298.457d, 6299.228d, 6302.0001d, 6302.7642d];

; I am not sure where this one came from: , 6304.53d

  atm_emi = [6300.304d]
  io_lines = [6300.304d]     ; I want to eventually put the Na in here

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

  dbopen,'io6300_integrated',0  
  io6300_entry = where_nday_eq(nday, $
                               COUNT=count,SILENT=silent, tolerance=0.002) 
  if count eq 0 then begin
     message, /continue, 'WARNING: unable to find database entry.  Doppler shifts will start at 0.'
  endif else begin              ; found entry in Melanie's database
     dbext, io6300_entry, 'deldot, rdot', deldots, rdots
     dopplers[id_io_dop] = double(deldots[0])
     dopplers[id_solar_dop] = dopplers[id_io_dop] + double(rdots[0])
  endelse                       ; found entry in Melanie's database
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
    message, 'ERROR: not enough DISPERS keyword found in header of ' + shortfile

  orig_dispers = dblarr(disp_order)
  orig_dispers = dispers[0:disp_order-1]
  dispers = orig_dispers

  window,7
  window,6
  title=string(objects[0], ' ', shortfile, ' ', dates[0])
  ;; This has to be in font !3 for the angstrom symbol to be found
  xtitle='Wavelength ('+string("305B)+')' ;" ;
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
  orig_err_spec = spec_errors[pix_axis]

  model_spec = fltarr(n_pix)

  left_wval = min(orig_wavelengths, a)
  left_idx = a[0]
  right_wval = max(orig_wavelengths, a)
  right_idx = a[0]

  pix_axis = orig_pix_axis
  ref_pixel = nx/2.

  ;; Need some initial starting structure or else IDL complains
  parinfo = ssg_init_parinfo(1)
  grave_params = [0.D]
  grave_parinfo = parinfo

  done = 0
  did_fit = 0
  saved = 0

  repeat begin
     
     ;; Extract useful things from parameter list, recording any
     ;; changes since the last iteration.  Also check to see if this
     ;; is the first time around or if we have deleted parameters
     ;; in order for them to be reinitialized.
     
     ;; DISPERSION
     disp_idx = where(parinfo.ssgID eq ssgid_disp, disp_order)
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
           tparinfo[idsp].limits = [double(p - 10d), $
                                    double(p + 10d)]
           tparinfo[idsp].parname=string(format='("Disp Coef ", i3)', idsp)
           tparinfo[idsp].ssgID=ssgid_disp
        endfor
        ;; check to see if we have any parameters
        if N_elements(params) le 1 then begin
           message, 'NOTE: initilizaing parameter list' ,/INFORMATIONAL
           params = tparams
           parinfo= tparinfo
        endif ;; Initilize parameter list
        message, 'NOTE: set dispersion coefficients to those found in FITS header of ' + shortfile ,/INFORMATIONAL
     endif ;; Dispersion initialization
     ;; Keep dispersion handy as a separate variable
     disp_idx = where(parinfo.ssgID eq ssgid_disp, disp_order)
     dispers = params[disp_idx]
     disp_parinfo = parinfo[disp_idx]

     ;; Handle the results of any wavelength window repositioning,
     ;; including recalculating the X axis for dispersion changes and
     ;; killing off or resurrecting lines 
     if left_idx gt right_idx then begin
        temp = left_idx & left_idx = right_idx & right_idx = temp
     endif
     left_wval =  orig_wavelengths[left_idx]
     right_wval = orig_wavelengths[right_idx]
     pix_axis = orig_pix_axis[left_idx:right_idx]
     xaxis = make_disp_axis(dispers, pix_axis, ref_pixel)
     spec = orig_spec[pix_axis]
     err_spec = orig_err_spec[pix_axis]
     orig_wavelengths = make_disp_axis(dispers, orig_pix_axis, ref_pixel)
     
     ;; DOPPLER SHIFTS. 
     dop_idx = where(parinfo.ssgID eq ssgid_dop, count)
     for idop=0,count-1 do $
       dopplers[parinfo[dop_idx[idop]].ssgdop] = $
       params[dop_idx[idop]]

     ;; If we have redshifted a line into our field, we need to
     ;; go back out to the blue to find it in our catalogs, so
     ;; calculate arrays of left and right wavelength limits for each
     ;; Doppler group.
     left_dop_wvals = left_wval  * ( 1 - dopplers / c )
     right_dop_wvals = right_wval  * ( 1 - dopplers / c )

     ;; Do a preliminary run of the model so we can recalculate the
     ;; observed wavelenghts
     model_spec = io_spec(pix_axis, params, parinfo=parinfo, $
                          ref_pixel=ref_pixel, spec=spec, err_spec=err_spec)
     
     ;; GRAVEYARD.  

     ;; Find the line center indexes of things that should potentially
     ;; be removed from the active line list
     grave_lc_idx = where(parinfo.ssgID ge 4 and $
                          parinfo.ssgowl ne 0 and $
                          (parinfo.ssgowl lt left_wval or $
                          right_wval lt parinfo.ssgowl), num_to_grave)
     for il=0,num_to_grave - 1 do begin
        ;; find all the parameters for this line
        cidx = grave_lc_idx[il] ; center idx
        message, 'NOTE: Looking at graveyard candidate ' + string(parinfo[cidx].parname), /CONTINUE
        myidx = where(parinfo.ssgID eq parinfo[cidx].ssgID and $
                      parinfo.ssggroupID eq parinfo[cidx].ssggroupID and $
                      (parinfo.ssgrwl eq parinfo[cidx].ssgrwl or $
                       NOT finite(parinfo[cidx].ssgowl)), count)
        if count lt 2 then $
          message, 'ERROR: Internal coding error not enough parameters found for this line'
        ;; Now calculate the model spectrum with just this line in it
        ;; and compare that to a model spectrum with the line in the
        ;; middle of the spectrum to see if we are losing too much area
        testpar=[dispers, params[myidx]]
        testparinfo=[disp_parinfo, parinfo[myidx]]
        
        model_spec = io_spec(pix_axis, testpar, parinfo=testparinfo, $
                             ref_pixel=ref_pixel, spec=spec, err_spec=err_spec)
        off_area = total(model_spec, /NAN)
        my_lc_idx = where(testparinfo.ssgowl ne 0, count)
        if count gt 1 or count eq 0 then begin
          message, 'WARNING: possible multiple entries in catalog ' + string(testparinfo[my_lc_idx[0]].ssggroupID) + ' for rest wavelength ' + string(testparinfo[my_lc_idx[0]].ssgrwl), /CONTINUE
          testparinfo[my_lc_idx].ssgowl = !values.f_nan          
       endif
        ;; There might be a better way to find the middle of the
        ;; spectrum in a general way, but this should be sufficient
        testparinfo[my_lc_idx[0]].ssgowl = (left_wval + right_wval)/2.
        model_spec = io_spec(pix_axis, testpar, parinfo=testparinfo, $
                             ref_pixel=ref_pixel, spec=spec, err_spec=err_spec)
        on_area = total(model_spec, /NAN)
        ;; This ratio is somewhat model and instrument profile
        ;; dependent, but I don't want to make it a command line
        ;; parameter just yet.  Split the difference between 0.5,
        ;; which is the symetric line on the edge case and something
        ;; really severe like 0.01, which would potentially be prone
        ;; to blowing up
        if off_area/on_area lt 0.05 then begin
           ;; This line belongs in the graveyard.  First check to see
           ;; if is there already
           message, 'NOTE: Sending ' + string(parinfo[cidx].parname) + ' to the graveyard', /CONTINUE
           my_grave_idx = $
             where(grave_parinfo.ssgID eq parinfo[cidx].ssgID and $
                   grave_parinfo.ssggroupID eq parinfo[cidx].ssggroupID and $
                   grave_parinfo.ssgrwl eq parinfo[cidx].ssgrwl, count)
           if count eq 0 then begin
              ;; Nope, not there append this line to the graveyard
              grave_params = [grave_params, params[myidx]]
              grave_parinfo = [grave_parinfo, parinfo[myidx]]
           endif else begin
              ;; Found it.  Replace this line in the graveyard
              grave_params[my_grave_idx] = params[myidx]
              grave_parinfo[my_grave_idx] = parinfo[myidx]
           endelse
           ;; In either case, mark line for removal from active list.
           ;; We are hoping the model has some other parameters like a
           ;; continuum, or else this could crash
           parinfo[myidx].ssgrwl = !values.f_NAN
        endif ;; Moved a line off to the graveyard
     endfor ;; Moving lines to the graveyard
     good_idx = where(finite(parinfo.ssgrwl) eq 1, count)
     ;; Ncomplement in where caused segmentataion fault!
     Ncomplement = N_elements(params) - count
     if count eq 0 then message, 'ERROR: there should always be dispersion and continuum parameters'
     if keyword_set(Ncomplement) then message, 'NOTE: deleting ' + string(Ncomplement) + ' parameters from the active list' , /continue
     temp = params [good_idx] & params  = temp
     temp = parinfo[good_idx] & parinfo = temp

     ;; Find the line center indexes of things that should potentially
     ;; be resurected from the graveyard
     resur_lc_idx = where(parinfo.ssgID ge 4 and $
                          left_wval lt grave_parinfo.ssgowl and $
                          grave_parinfo.ssgowl lt right_wval, num_resur)
     for il=0,num_resur - 1 do begin
        ;; find all the parameters for this line
        cidx = resur_lc_idx[il] ; center idx
        message, 'NOTE: Looking at resurrection candidate ' + string(grave_parinfo[cidx].parname), /CONTINUE
        myidx = $
          where(grave_parinfo.ssgID eq grave_parinfo[cidx].ssgID and $
                grave_parinfo.ssggroupID eq grave_parinfo[cidx].ssggroupID and $
                (grave_parinfo.ssgrwl eq grave_parinfo[cidx].ssgrwl or $
                NOT finite(grave_parinfo[cidx].ssgowl)), count)
        if count lt 2 then $
          message, 'ERROR: Internal coding error not enough parameters found for this line'
        ;; Now calculate the model spectrum with just this line in it
        ;; and compare that to a model spectrum with the line in the
        ;; middle of the spectrum to see if we have gained back enough
        ;; area to add the line
        testpar=[dispers, grave_params[myidx]]
        testparinfo=[disp_parinfo, grave_parinfo[myidx]]
        model_spec = io_spec(pix_axis, testpar, parinfo=testparinfo, $
                             ref_pixel=ref_pixel, spec=spec, err_spec=err_spec)
        off_area = total(model_spec, /NAN)
        my_lc_idx = where(testparinfo.ssgowl ne 0, count)
        if count gt 1 or count eq 0 then begin
           message, 'WARNING: possible multiple entries for rest wavelength ' + string(testparinfo[my_lc_idx[0]].ssgrwl), /CONTINUE
           testparinfo[my_lc_idx].ssgowl = !values.f_nan          
        endif
        ;; There might be a better way to find the middle of the
        ;; spectrum in a general way, but this should be sufficient
        testparinfo[my_lc_idx[0]].ssgowl = (left_wval + right_wval)/2.
        model_spec = io_spec(pix_axis, testpar, parinfo=testparinfo, $
                             ref_pixel=ref_pixel, spec=spec, err_spec=err_spec)
        on_area = total(model_spec, /NAN)
        ;; SEE DOCUMENTATION ABOVE.  But it is a little harder to do
        ;; on the way out, since I use the window edge as the trigger
        if off_area/on_area ge 0.5 then begin
           message, 'NOTE: Resurrecting ' + string(grave_parinfo[cidx].parname), /CONTINUE
           ;; This line should be resurrected.  First check to see if is
           ;; there already
           idx = where(parinfo.ssgID eq grave_parinfo[cidx].ssgID and $
                       parinfo.ssggroupID eq $
                       grave_parinfo[cidx].ssggroupID and $
                       parinfo.ssgrwl eq grave_parinfo[cidx].ssgrwl, $
                       count)
           if count eq 0 then begin
              ;; Append this line to the regular parameter list
              params =  [params,  grave_params[myidx]]
              parinfo = [parinfo, grave_parinfo[myidx]]
           endif
           ;; Remove line from graveyard
           grave_parinfo[myidx].ssgrwl = !values.f_NAN
        endif ;; Resurrected a line
     endfor ;; resurrecting lines
     good_idx = where(finite(grave_parinfo.ssgrwl) eq 1, count)
     ;; Ncomplement in where caused segmentataion fault!
     Ncomplement = N_elements(grave_params) - count

     if count eq 0 then message, 'ERROR: there should always be an initial 0 graveyard entry'
     if keyword_set(Ncomplement) then message, 'NOTE: deleting ' + string(Ncomplement) + ' parameters from graveyard' , /continue
     temp = grave_params [good_idx] & grave_params  = temp
     temp = grave_parinfo[good_idx] & grave_parinfo = temp

     ;; Now I have to decide if I am going to automatically add lines
     ;; as they com into the window.  I vote for no, since the user
     ;; can do that + the graveyard should be a handy resource.
     

     ;; CONTINUUM.  It is important to do it in this order so the
     ;; median spectrum is taken from the stuff in the window, not the
     ;; whole spectrum
     cont_idx = where(parinfo.ssgID eq ssgid_cont, N_continuum)
     if N_continuum eq 0 then begin
        message, 'NOTE: resetting continuum to median of displayed spectrum and, if N_continuum specified on the command line, zeroing higher order terms ', /INFORMATIONAL
        m = median(orig_spec)
        tparams = dblarr(N_continuum_orig)
        tparams[0] = m
        tparinfo = ssg_init_parinfo(N_continuum_orig)
        for ic = 0,N_continuum_orig-1 do begin
           p = tparams[ic] 
           ;; --> I have no idea if these are reasonable absolute boundardies
           tparinfo[ic].limits = [double(p - 1.d), $
                                  double(p + 1.d)]
           tparinfo[ic].parname = string(format='("Cont. Poly Coef ", i3)', ic)
           tparinfo[ic].vfID = vfid_cont
           tparinfo[ic].ssgID = ssgid_cont
        endfor
        ;; Limits on 0th order should be broad they can be constrained
        ;; by the used later.  --> NOTE: this assumes a positive
        ;; continuum + could be improved if necessary, but I don't
        ;; want to use min/max(spec) unless I multiply it by something
        ;; or add the median, since the true continuum might be really
        ;; high or low....
        tparinfo[0].limits = [-m, 2.D*m]
        params = [params, tparams]
        parinfo = [parinfo, tparinfo]
     endif ;; Continuum initialization
     ;; Keep continuum handy as a separate variable
     cont_idx = where(parinfo.ssgID eq ssgid_cont, N_continuum)
     continuum = params[cont_idx]

     ;; --> I should do a limit check here just to keep mpfit happy

     model_spec = io_spec(pix_axis, params, parinfo=parinfo, $
                          ref_pixel=ref_pixel, spec=spec, err_spec=err_spec)
     
     residual = spec - model_spec
     chisq = total(residual^2, /NAN)
     free_idx = where(parinfo.fixed ne 1, nfree)
     redchisq = chisq/(nfree - 1)
     ;; Make sure we have some sort of error list
     if N_elements(perrors) ne N_elements(params) then $
       perrors = dblarr(N_elements(params))
     
     wset,6
     !p.multi = [0,0,2]

     plot, xaxis, spec, psym=dot, title=title, xtitle=xtitle, ytitle=ytitle, $
       xrange=[min(xaxis), max(xaxis)], yrange=[min(spec), max(spec)], $
       xstyle=1, ystyle=2
     oploterr, xaxis, spec, err_spec, dot
     oplot, xaxis, model_spec, linestyle=dotted

     plot, xaxis, residual, $
       title='Fit residual', xtitle=xtitle, ytitle=ytitle, $
       xstyle=1, ystyle=2, psym=10
     !p.multi = 0
     message, /CONTINUE, 'Use left and right buttons select bracket a region of interest.  Middle button brings up menu.'

     cursor, x1, y1, /DOWN, /DATA
     ;; Left mouse
     if !MOUSE.button eq 1 then begin
        dxs = abs(orig_wavelengths - x1)
        junk = min(dxs, a)
        left_idx = a[0]
     endif ;; leftmost mouse button

     ;; Right mouse
     if !MOUSE.button eq 4 then begin
        dxs = abs(orig_wavelengths - x1)
        junk = min(dxs, a)
        right_idx = a[0]
     endif ;; rightmost mouse button

     ;; Middle mouse
     if !MOUSE.button eq 2 then begin
        message, /CONTINUE, 'Menu:'
        print, 'Fit '
        print, 'reNormalize lines one by one '
        print, 'Modify parameters'
        print, 'unZoom'
        print, 'Save in database'
        print, 'Read from database'
        print, 'Quit'
        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, '[F], N, M, R, Z, S, Q?'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'F'
           for ki = 0,1000 do flush_input = get_kbrd(0)
           answer = strupcase(answer)
        endrep until $
          answer eq 'F' or $
          answer eq 'N' or $
          answer eq 'M' or $
          answer eq 'R' or $
          answer eq 'Z' or $
          answer eq 'S' or $
          answer eq 'Q'

        if answer eq 'M' then begin
           message, /CONTINUE, 'Modify parameters menu:'
           print, 'Add lines '
           print, 'Remove lines'
           print, 'modify 1 Line'
           print, 'modify Doppler shifts'
           print, 'change disperSion relation'
           print, 'change continuum Order'
           print, 'Quit this menu'
           answer = ''
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'A, R, L, D, S, O, [Q]'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'Q'
              answer = strupcase(answer)
              for ki = 0,1000 do flush_input = get_kbrd(0)
           endrep until $
             answer eq 'A' or $
             answer eq 'R' or $
             answer eq 'L' or $
             answer eq 'D' or $
             answer eq 'S' or $
             answer eq 'O' or $
             answer eq 'Q'

           ;; Save off exising parameter stuff so we can recover values
           old_params = params
           old_parinfo = parinfo

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

           ;; Change continuum polynomial order 
           if answer eq 'O' then begin
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
              message, /CONTINUE, 'Line lists (only S is case sensitive):'
              print, 'Strong solar lines (S) '
              print, 'Weak solar lines (s)'
              print, 'Atmospheric Absoption lines (A)'
              print, 'atmospheric Emission lines (E)'
              print, 'Io line'
              print, 'Random line by hand'
              print, 'Quit this menu (Q or q)'
              answer = ''
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'S, s, A, E, I, R, [Q]'
                 answer = get_kbrd(1)
                 if byte(answer) eq 10 then answer = 'Q'
                 uanswer = strupcase(answer)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until $
                uanswer eq 'S' or $
                uanswer eq 'A' or $
                uanswer eq 'E' or $
                uanswer eq 'I' or $
                uanswer eq 'R' or $
                uanswer eq 'Q'
              
              ;; Strong solar lines
              if answer eq 'S' then begin
                 good_idx = where(parinfo.ssggroupID ne 2, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; Weak solar lines
              if answer eq 's' then begin
                 good_idx = where(parinfo.ssggroupID ne 3, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; Strong atmospheric lines
              if uanswer eq 'A' then begin
                 good_idx = where(parinfo.ssggroupID ne 4, count)
                 if count gt 0 then begin
                    temp = params[good_idx] & params = temp
                    temp = parinfo[good_idx] & parinfo = temp
                 endif
              endif
              ;; Weak atmospheric lines
              if uanswer eq 'E' then begin
                 good_idx = where(parinfo.ssggroupID ne 5, count)
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
              message, /CONTINUE, 'Line lists (only S is case sensitive):'
              print, 'Strong solar lines (S) '
              print, 'Weak solar lines (s)'
              print, 'Atmospheric Absorption lines (A)'
              print, 'atmospheric Emission lines (E)'
              print, 'Io line'
              print, 'Random line by hand'
              print, 'Quit this menu (Q or q)'
              answer = ''
              for ki = 0,1000 do flush_input = get_kbrd(0)
              repeat begin
                 message, /CONTINUE, 'S, s, A, E, I, R, [Q]'
                 answer = get_kbrd(1)
                 if byte(answer) eq 10 then answer = 'Q'
                 uanswer = strupcase(answer)
                 for ki = 0,1000 do flush_input = get_kbrd(0)
              endrep until $
                uanswer eq 'S' or $
                uanswer eq 'A' or $
                uanswer eq 'E' or $
                uanswer eq 'I' or $
                uanswer eq 'R' or $
                uanswer eq 'Q'
              
              if uanswer eq 'S' then begin
                 ;; Check to see if we need to add a solar doppler
                 ;; shift parameter
                 sd_idx = where(parinfo.ssgID eq ssgid_dop $
                                and parinfo.ssgdop eq id_solar_dop, count)
                 if count eq 0 then begin
                    tparam=dopplers[id_solar_dop]
                    tparinfo = ssg_init_parinfo(1)
                    ;; --> Again, a wild guess, assuming we are in
                    ;; km/s, etc.
                    tparinfo.limits = [tparam-100,tparam+100]
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
                 ;; Check to see if we need to add a solar doppler
                 ;; shift parameter
                 iod_idx = where(parinfo.ssgID eq ssgid_dop $
                                 and parinfo.ssgdop eq id_Io_dop, count)
                 if count eq 0 then begin
                    tparam=dopplers[id_Io_dop]
                    tparinfo = ssg_init_parinfo(1)
                    ;; --> Again, a wild guess, assuming we are in
                    ;; km/s, etc.
                    tparinfo.limits = [tparam-100,tparam+100]
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
                       ew = 0.005d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, 0.08, 0., 1.]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-ew, ew]
                       tparinfo[1].limited = [1,1]
                       tparinfo[1].limits  = [0.01d, 0.15d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [0.d, 0.01d]
                       tparinfo[3].limited = [1,0]
                       tparinfo[3].limits  = [0.d, 100]
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
              if answer eq 'S' then begin
                 solar_idx = $
                   where(left_dop_wvals[id_solar_dop] le solar_atlas.lambdaD[*] and $
                         solar_atlas.lambdaD[*] le right_dop_wvals[id_solar_dop], $
                         n_solar)
                 ;; For each strong solar line
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
                       area = -exp(solar_atlas.loggf[solar_idx[isl]]) * continuum[0]/3.
                       tparams = [wl, 0.2, 0., area]
                       
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-ew, ew]
                       tparinfo[1].limited = [1,1]
                       tparinfo[1].limits  = [0.02d, 0.3d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [0.d, 0.25d]
                       tparinfo[3].limited = [0,1]
                       tparinfo[3].limits  = [-10.d*area, 0.d]
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

              ;; Weak solar lines
              if answer eq 's' then begin
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
                    old_idx = where(old_parinfo.ssggroupID eq 3 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       ew = 0.005d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, 0.2, 0., -1.]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-ew, ew]
                       tparinfo[1].limited = [1,1]
                       tparinfo[1].limits  = [0.02d, 0.3d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [0.d, 0.25d]
                       tparinfo[3].limited = [0,1]
                       tparinfo[3].limits  = [-3, 0.d]
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
                 endfor ;; Weak solar line loop
              endif ;; adding weak solar line

              ;; Atmospheric absorption lines
              if uanswer eq 'A' then begin
                 atm_idx = where(left_wval le atm_absorb and $
                                 atm_absorb le right_wval, $
                                 n_atm)
                 ;; For absoprtion feature
                 for ial = 0, n_atm-1 do begin
                    ;; Get its wavelength and check to see if it is
                    ;; already in our paramter list
                    wl = atm_absorb[atm_idx[ial]]
                    old_idx = where(old_parinfo.ssggroupID eq 4 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       ew = 0.005d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, 0.1, 0., -2]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-ew, ew]
                       tparinfo[1].limited = [1,1]
                       tparinfo[1].limits  = [0.005d, 0.3d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [0.d, 0.25d]
                       tparinfo[3].limited = [0,1]
                       tparinfo[3].limits  = [-10.D, 0]
                       tparinfo.ssggroupID[*] = 4
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
                    old_idx = where(old_parinfo.ssggroupID eq 5 and $
                                    old_parinfo.ssgrwl eq wl, count)
                    if count eq 0 then begin
                       ;; We need to add a new line --> worry about
                       ;; deleting lines or fixing their parameters later
                       ew = 0.005d ; Might want to change this
                       ;; Take a wild guess at the initial parameters
                       tparams = [wl, 0.05, 0., 0.05]
                       tparinfo = ssg_init_parinfo(voigt=tparams)
                       tparinfo[0].limited = [1,1]
                       tparinfo[0].limits  = [-ew, ew]
                       tparinfo[1].limited = [1,1]
                       tparinfo[1].limits  = [0.005d, 0.3d]
                       tparinfo[2].limited = [1,1]
                       tparinfo[2].limits  = [0.d, 0.25d]
                       tparinfo[3].limited = [1,0]
                       tparinfo[3].limits  = [0,2.d]
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
                    endif ;; Initialize an atmospheric emission line
                 endfor ;; Atmospheric emission
              endif ;; adding atmospheric emission line

           endif ;; Adding lines menu of modifying parameters

           ;; Last thing: display parameters
           ssg_display_params, params, parinfo
           ;; Reset answer so it doesn't trigger other menus
           answer = ''
        endif ;; Modify parameters 'M'

        if answer eq 'F' then begin
           old_params = params
           old_parinfo = parinfo
           err=0
           CATCH, err
           if err ne 0 then begin
              message, /NONAME, !error_state.msg, /CONTINUE
              message, 'WARNING: error detected, reseting parameters to previous values', /CONTINUE
              params =  old_params
              parinfo = old_parinfo
           endif else begin

              to_pass = {parinfo:parinfo, ref_pixel:ref_pixel, spec:spec, $
                         err_spec:err_spec}
              params = mpfitfun('io_spec', pix_axis, spec, err_spec, $
                                params, FUNCTARGS=to_pass, AUTODERIVATIVE=1, $
                                PARINFO=parinfo, maxiter=maxiter, $
                                BESTNORM=chisq, PERROR=perrors, STATUS=status)

              ssg_display_params, params, parinfo
              message, 'MPFITFUN returned STATUS ' + string(status), /CONTINUE
              io_idx = where(parinfo.ssggroupID eq 1, count)
              if count eq 0 then begin
                 message, /continue, 'WARNING: No Io parameters'
              endif else begin
                 dop_idx = where(parinfo.ssgID eq ssgid_dop and $
                                 parinfo.ssgdop eq id_Io_dop)
                 message, 'Io ephemeris Doppler shift is ' + string(deldots[0]) + ' measured is ' + string(params[dop_idx[0]]), /CONTINUE
              endelse
              if status eq 5 then $
                message, 'WARNING: STATUS 5 menas MPFITFUN failed to converge after MAXITER (' + string(maxiter) + ') iterations', /CONTINUE
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
           all_but_areas = where(parinfo.vfID ne 5, complement=area_idx)
           parinfo[all_but_areas].fixed = 1

           ;; Now for each line do an mpfitfun
           for il=0, n_elements(area_idx)-1 do begin
              parinfo[area_idx].fixed = 1
              parinfo[area_idx[il]].fixed = 0
              to_pass = {parinfo:parinfo, ref_pixel:ref_pixel, spec:spec, $
                         err_spec:err_spec}
              params = mpfitfun('io_spec', pix_axis, spec, err_spec, $
                                params, FUNCTARGS=to_pass, AUTODERIVATIVE=1, $
                                PARINFO=parinfo, maxiter=10)
           endfor
           parinfo.fixed = old_fixed
           CATCH, /CANCEL
           did_fit = 1
           saved = 0
        endif ;; Renormalize

        if answer eq 'Z' then begin
           left_idx = min(orig_pix_axis)
           right_idx = max(orig_pix_axis)
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
