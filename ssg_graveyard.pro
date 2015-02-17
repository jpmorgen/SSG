;+
; NAME: ssg_graveyard
;
;
;
; PURPOSE: Marks lines in a parinfo list as inactive if they do not
; contribute significantly to the model function in the wavelength window
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:  
;
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
; $Id: ssg_graveyard.pro,v 1.1 2015/02/17 23:04:07 jpmorgen Exp $
;-

pro ssg_graveyard, pix_axis, parinfo, pix_limits=pix_limits, $
                   min_ew=min_ew, small_ew=small_ew, $
                   spec=spec, err_spec=err_spec, $
                   maxprint=maxprint, quiet=quiet

  minpix = min(pix_axis, /NAN)
  maxpix = max(pix_axis, /NAN)

  if N_elements(pix_limits) eq 0 then $
    pix_limits = [minpix, maxpix]
  if N_elements(pix_limits) lt 2 then $
    message, 'ERROR: pix_limits must be a two-element vector (min, max)'
  if pix_limits[0] lt minpix then $
    pix_limits[0] = minpix
  if pix_limits[1] gt maxpix then $
    pix_limits[1] = maxpix

  ;; Get handy indices
  disp_idx = where(parinfo.pfo.inaxis eq !pfo.Xin and $
                   parinfo.pfo.outaxis eq !pfo.Xaxis, ndisp)
  cont_idx = where(parinfo.sso.ptype eq !sso.cont, ncont)
  dop_idx = where(parinfo.sso.ptype eq !sso.dop, ndop)

  if ndop eq 0 or ncont eq 0 or ndisp eq 0 then $
    message, 'ERROR: parinfo is missing necessary parameters.  Must have dispersion definition, Doppler shift, and continuum level parameters'

  cont_spec = pfo_funct(pix_axis[pix_limits[0]:pix_limits[1]], $
                        parinfo=parinfo, idx=[disp_idx, cont_idx])

  if N_elements(min_ew) eq 0 then begin
     ;; area = cont * ew
     ;;
     ;; A minimum area is the median errorbar times the
     ;; dispersion coefficient *2, assuming Nyquist sampling.

     ldisp = parinfo[sso_get_disp_idx(parinfo, idx=disp_idx)].value
     ;; It is a little hard to decide what to use for a continuum when
     ;; it is complicated.  I think the median is the best bet under
     ;; the circumstances
     cont = median(cont_spec)
     err = median(err_spec[pix_axis])
     ;; Don't forget to convert to mA and make a 5 sigma minimum
     ;; detection limit
     min_ew = 2d * ldisp * err / cont / !sso.lwcvt * 2d
print, 'MIN_EW = ', min_ew

  endif

  if N_elements(small_ew) eq 0 then begin
     ;; area = cont * ew
     ;;
     ;; A minimum area is the median errorbar times the
     ;; dispersion coefficient *2, assuming Nyquist sampling.

     ldisp = parinfo[sso_get_disp_idx(parinfo, idx=disp_idx)].value
     ;; It is a little hard to decide what to use for a continuum when
     ;; it is complicated.  I think the median is the best bet under
     ;; the circumstances
     cont = median(cont_spec)
     err = median(err_spec[pix_axis])
     ;; Don't forget to convert to mA
     small_ew = 2d * ldisp * err / cont / !sso.lwcvt * 20d
print, 'SMALL_EW = ', small_ew

  endif

  ;; Do a preliminary run of the model so we can recalculate the
  ;; observed wavelenghts so the graveyard works properly
  model_spec = pfo_funct(pix_axis, parinfo=parinfo, xaxis=wavelengths)

  left_wval =  wavelengths[pix_limits[0]]
  right_wval = wavelengths[pix_limits[1]]

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

  if num_to_grave gt maxprint and NOT keyword_set(quiet) then $
    message, /INFORMATIONAL, 'NOTE: sending approx ' + strtrim(num_to_grave, 2) + ' lines outside the active region to the graveyard.'
  for il=0,num_to_grave - 1 do begin
     ;; find all the parameters for this line
     c_idx = lc_idx[grave_lc_idx[il]] ; center idx
     dg = parinfo[c_idx].sso.dg ; Doppler group
     rwl = parinfo[c_idx].sso.rwl ; rest wavelength
     if num_to_grave le maxprint and NOT keyword_set(quiet) then $
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
     model_spec = pfo_funct(pix_axis[pix_limits[0]:pix_limits[1]], $
                            parinfo=parinfo, $
                            idx=[disp_idx, cont_idx, dop_idx, myidx])

     parinfo[myidx].sso.rwl = rwl
     model_spec = model_spec - cont_spec
     on_area = total(model_spec, /NAN)

     ;; OFF AREA
     model_spec = pfo_funct(pix_axis[pix_limits[0]:pix_limits[1]], $
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
        if num_to_grave le maxprint and NOT keyword_set(quiet) then $
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

  if num_resur gt maxprint and NOT keyword_set(quiet) then $
    message, 'NOTE: Resurrecting aprox ' + strtrim(num_resur, 2) + ' lines that are now in the active region.', /INFORMATIONAL
  for il=0,num_resur - 1 do begin
     ;; find all the parameters for this line
     c_idx = lc_idx[resur_lc_idx[il]] ; center idx
     dg = parinfo[c_idx].sso.dg ; Doppler group
     rwl = parinfo[c_idx].sso.rwl ; rest wavelength
     if num_resur le maxprint and NOT keyword_set(quiet) then $
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

     model_spec = pfo_funct(pix_axis[pix_limits[0]:pix_limits[1]], $
                            parinfo=parinfo, $
                            idx=[disp_idx, cont_idx, dop_idx, myidx])
     parinfo[myidx].sso.rwl = rwl

     model_spec = model_spec - cont_spec
     on_area = total(model_spec, /NAN)

     ;; OFF AREA
     model_spec = pfo_funct(pix_axis[pix_limits[0]:pix_limits[1]], $
                            parinfo=parinfo, $
                            idx=[disp_idx, cont_idx, dop_idx, myidx])

     model_spec = model_spec - cont_spec
     off_area = total(model_spec, /NAN)

     ;; SEE DOCUMENTATION ABOVE.  But it is a little harder to do
     ;; on the way out, since I use the window edge as the trigger
     if off_area - on_area ne 0 and off_area/on_area ge 0.5 then begin
        ;; In order to do the calculation, this line was already
        ;; resurrected.
        if num_resur le maxprint and NOT keyword_set(quiet) then $
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
     dg = parinfo[idx].sso.dg   ; Doppler group
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
  if N_elements(zero_lor_idx) gt maxprint and NOT keyword_set(quiet) then begin
     message, /INFORMATIONAL, 'NOTE: zeroing Lorentzian width of ' + strtrim(N_elements(zero_lor_idx),2) + ' lines because their equivalent widths are below ' + strtrim(small_ew, 2)
  endif else begin
     if finite(zero_lor_idx[0]) then begin
        for ilor=0, N_elements(zero_lor_idx)-1 do begin
           message, /INFORMATIONAL, 'NOTE: zeroing Lorentzian width of ' + strjoin(sso_dg_path(parinfo[zero_lor_idx[ilor]].sso.dg, /name), '-') + ' ' + string(format=!sso.rwl_format, parinfo[zero_lor_idx[ilor]].sso.rwl) + ' because its equivalent width is below ' + strtrim(small_ew, 2)
        endfor
     endif
  endelse

  if N_elements(free_lor_idx) gt maxprint and NOT keyword_set(quiet) then begin
     message, /INFORMATIONAL, 'NOTE: freeing Lorentzian width of ' + strtrim(N_elements(free_lor_idx),2) + ' lines because their equivalent widths are above ' + strtrim(small_ew, 2)
  endif else begin
     if finite(free_lor_idx[0]) then begin
        for ilor=0, N_elements(free_lor_idx)-1 do begin
           message, /INFORMATIONAL, 'NOTE: freeing Lorentzian width of ' + strjoin(sso_dg_path(parinfo[free_lor_idx[ilor]].sso.dg, /name), '-') + ' ' + string(format=!sso.rwl_format, parinfo[free_lor_idx[ilor]].sso.rwl) + ' because its equivalent width is above ' + strtrim(small_ew, 2)
        endfor
     endif
  endelse
  ;; End of GRAVEYARD and RESURRECTION stuff



end
