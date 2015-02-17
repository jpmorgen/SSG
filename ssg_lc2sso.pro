; +
;; $Id: ssg_lc2sso.pro,v 1.1 2015/02/17 23:04:24 jpmorgen Exp $

;; ssg_lc2sso.pro
;;
;; Convert a set of lc records into a valid sso parinfo array handling
;; specifics for the ssg.

; -

function ssg_lc2sso, lc, ftype, idx=idx, center=center, area=area, $
                     width=width, no_lc_area=no_lc_area, $
                     no_lc_width=no_lc_width, no_check=no_check, $
                     _EXTRA=extra

  init = {ssg_sysvar}

  sso = lc2sso(lc, ftype!pfo.voigt, idx=sun_idx, value=[0,0,150,10], $
               path=[!eph.sun, !eph.io, !eph.earth], $
               parinfo_template=!ssg.parinfo, /no_check)
  
  ;; Error checking and defaults
  if N_elements(lc) eq 0 then $
    message, 'ERROR: lc not specified'
  if N_elements(idx) eq 0 then $
    idx = lindgen(N_elements(lc))
  if N_elements(ftype) eq 0 then $
    ftype = !pfo.voigt
  ;; Specify center, area, and width transformation flags explicitly
  ;; so it doesn't get stuck in _EXTRA and raise an error in *f_mod.
  ;; By default assume we want all three transformations.
  if N_elements(center) eq 0 then $
    center = 1
  if N_elements(area) eq 0 then $
    area = 1
  if N_elements(width) eq 0 then $
    width = 1

  ;; Find out what one element of this function looks like (without
  ;; Doppler shifts).  Make sure to get the center marked so we can
  ;; assign at least rest wavelengths.  We can undo this later.
  parinfo1 = sso_fcreate(!sso.line, sso_ftype=ftype, /center, $
                         area=area, width=width, /no_check, _EXTRA=extra)
  npar1 = N_elements(parinfo1)


  ;; Set unspecified limits to NAN so we can spot any changes lc makes
  for il=0,1 do begin
     bad_idx = where(parinfo1.limited[il] eq 0, nbad)
     if nbad gt 0 then begin
        parinfo1[bad_idx].limits[il] = !values.d_nan
     endif
  endfor ;; limits 0,1

  ;; Find out how many lines we have in the catalog (some lc records
  ;; might be widths)
  lcw_idx = where(lc[idx].ptype eq !lc.wavelen, nfns)
  ;; unnest
  lcw_idx = idx[lcw_idx]

  ;; Quickly create the right number of parinfo records
  parinfo = replicate(parinfo1[0], nfns*npar1)

  ;; Go back and set up the function stuff properly.

  ;; RWL is defined for all the parameters in a function
  for ifn=0, nfns-1 do begin
     for i1=0, npar1-1 do begin
        parinfo[ifn*npar1 + i1] = parinfo1[i1]
        parinfo[ifn*npar1 + i1].sso.rwl = lc[lcw_idx[ifn]].value
     endfor
  endfor

  ;; WAVELENGTH.  We have [hopefully] marked the centers with sso_fcreate
  lc_idx = where(parinfo.sso.ttype eq !sso.center, nlc)
  if nlc eq 0 then $
    message, 'ERROR: no line centers found in the parinfo structure!'
  if nlc ne nfns then $
    message, 'ERROR: the number of line centers and the number of functions in parinfo do not match.  nlc = ' + strtrim(nlc, 2) + ' nfns = ' + strtrim(nfns, 2)

  if center eq 0 then begin
     ;; We want conventional fits using the wavelengths themselves.
     ;; It is faster to loop through things explicitly or use pfo_fmod
     ;; or mpfit_pfo_mod, but I would rather this code, which only
     ;; runs occationally, be easy to read than fast.
     sso_fmod, parinfo, lc_idx, value=parinfo[lc_idx].sso.rwl
     sso_fmod, parinfo, lc_idx, limits=lc[lcw_idx].limits     
     ;; If we really didn't want to use the rwl system
     sso_fmod, parinfo, lc_idx, center=0
  endif else begin
     ;; We have the standard rwl system: adjust the wavelength limits
     ;; accordingly.
     limits = lc[lcw_idx].limits
     limits[0,*] = limits[0,*] - lc[lcw_idx[*]].value
     limits[1,*] = limits[1,*] - lc[lcw_idx[*]].value
     ;; And apply the wd conversion factor
     limits[0:1,*] = limits[0:1,*] / !sso.dwcvt
     sso_fmod, parinfo, lc_idx, limits=limits
  endelse

  ;; --> this code could be modularized

  ;; equivalent width
  lceqw_idx = where(lc[idx].ptype eq !lc.ew, neqw)
  if neqw gt 0 and NOT keyword_set(no_lc_area) then begin
     ;; unnest line catalog indices
     lceqw_idx = idx[lceqw_idx]
     ;; Find eq width records in parinfo.
     peqw_idx = where(parinfo.sso.ttype eq !sso.area, neqw)
     ilceqw = 0
     for ifn=0, neqw-1 do begin
        ;; Make sure wavelengths match.  Here is where we assume that
        ;; lc is sorted to save time.  We can use eq since the real
        ;; numbers should be identical bit-for-bit.
        if lc[lceqw_idx[ilceqw]].wavelen eq $
          parinfo[peqw_idx[ifn]].sso.rwl then begin
           ;; --> here we assume whatever transformation (e.g. milli
           ;; Angstroms) has been applied to the line catalog
           parinfo[peqw_idx[ifn]].value = lc[lceqw_idx[ilceqw]].value
           parinfo[peqw_idx[ifn]].limits = lc[lceqw_idx[ilceqw]].limits
           ilceqw = ilceqw + 1
        endif ;; Wavelengths match
     endfor ;; Each parinfo equivalent width record
  endif ;; equivalent widths

  ;; Linewidth.  Handle only Gaussian here.
  lclw_idx = where(lc[idx].ptype eq !lc.gw, nlw)
  if nlw gt 0 and NOT keyword_set(no_lc_width) then begin
     junk = where(parinfo1.sso.ptype eq !sso.width, count)
     if count gt 1 then $
       message, 'ERROR: Target function type ' + !pfo.fnames[ftype] + ' has more than one linewidth parameter (e.g. Gaussian and Lorentzian).  You will have to write a specific funciton to be able to handle this case (e.g. ssg_lc2sso).  To avoid this error message and extract the rest of the stuff you were hoping for, specify /no_lc_width'
     ;; unnest line catalog indices
     lclw_idx = idx[lclw_idx]
     ;; Find width records in parinfo.  This keys in on the generic
     ;; width transformation flag set by sso.  If you know what pfo
     ;; primitive you are using, you can select based on the decimal
     ;; ftype (note: make sure to take rounding errors into
     ;; consideration -- see pfo_poly for examples).
     plw_idx = where(parinfo.sso.ttype eq !sso.width, nlw)
     ilclw = 0
     for ifn=0, nlw-1 do begin
        ;; Make sure wavelengths match.  Here is where we assume that
        ;; lc is sorted to save time.  We can use eq since the real
        ;; numbers should be identical bit-for-bit.
        if lc[lceqw_idx[ilclw]].wavelen eq $
          parinfo[peqw_idx[ifn]].sso.rwl then begin
           ;; --> here we assume whatever transformation (e.g. milli
           ;; Angstroms) has been applied to the line catalog
           parinfo[plw_idx[ifn]].value = lc[lclw_idx[ilclw]].value
           parinfo[plw_idx[ifn]].limits = lc[lclw_idx[ilclw]].limits
           ilclw = ilclw + 1
        endif
     endfor ;; Each parinfo linewidth record
  endif ;; Linewidths

  for il=0,1 do begin
     good_idx = where(finite(parinfo.limits[il]), ngood, complement=bad_idx, $
                     ncomplement=nbad)
     if ngood gt 0 then begin
        parinfo[good_idx].limited[il] = 1
     endif
     if nbad gt 0 then begin
        parinfo[bad_idx].limits[il] = 0 ; unset NAN value from catalog
     endif
  endfor ;; limits 0,1

  if NOT keyword_set(no_check) then $
    sso_fcheck, parinfo

  return, parinfo

end

