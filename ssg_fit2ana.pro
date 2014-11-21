;+
; $Id: ssg_fit2ana.pro,v 1.3 2014/11/21 02:43:00 jpmorgen Exp $

; ssg_fit2ana.  Takes information from the fit database and stuffs it
; into the analysis database

;-

pro ssg_fit2ana, nday_start_or_range, interactive=interactive


  init = {ssg_sysvar}

;  ON_ERROR, 2
  oldpriv=!priv
  !priv = 2
  ;;CATCH, err
  ;;if err ne 0 then begin
  ;;   message, /NONAME, !error_state.msg, /CONTINUE
  ;;   message, 'Closing database(s) and exiting gracefully',/CONTINUE
  ;;   dbclose
  ;;   !priv = oldpriv
  ;;   return
  ;;endif

  if NOT keyword_set(nday_start_or_range) then nday_start_or_range=0

  c = 299792.458 ;; km/s

  message, /CONTINUE, 'Select observations from which to extract Io intensity measurements'
  ndays=ssg_select(nday_start_or_range, count=count, $
                   title='Select spectra to transfer to analysis database', $
                   non_interactive=(NOT keyword_set(interactive)))
  if count eq 0 then return
  rdbname = 'ssg_reduce'
;  fdbname = 'oi_6300_fit'
;  adbname = 'io6300_integrated'
  adbname = 'io_oi_analyze'

  ;; Possibly phasing out the fit database in favor of parinfo.sav files
  dbopen, rdbname
  entries = where_nday_eq(ndays, count=N_ndays, tolerance=0.001)

  dbext, entries, 'dir, disp_pix, spectrum, spec_err', $
         dirs, disp_pix, spectra, spec_errors
  dbclose

  dbopen, adbname, 0
  aentries = where_nday_eq(ndays, count=adays, tolerance=0.001)
  if N_ndays ne adays then $
    message, 'ERROR: internal database weirdness'

   if count ne adays then begin
     message, 'WARNING: fitting and analysis databases are not lining up properly, using the analysis as the master', /CONTINUE
     dbext, aentries, 'nday', ndays
  endif

  ;; Collect things we need for our calculations
  dbext, aentries, 'delta,r,phi,spa,ang_dia', delta, r, phi, sol_pha, io_dia
  ;; Collect arrays for our output results
  dbext, aentries, $
         'deldot_m,err_deldot_m, fline,err_fline, fcont,err_fcont, wc,err_wc', $
         dv, err_dv, fline, err_fline, fcont, err_fcont, wc, err_wc

  dbext, aentries, $
         'ag_flux,err_ag_flux, redchisq,freeparam,numlines,db_date,disp,refpix, refwave', $
         ag, ag_err, redchisq, nfree, numlines, today, disp, refpix, refwave

  dbext, aentries, $
         'weq, err_weq, alf, err_alf, p_date, intensity, err_intensity', $
         weq, err_weq, alf, err_alf, today, intensity, err_intensity

  dbclose

  ;; Mark all variables with NAN, or the typecast equivalent so we
  ;; know what gets filled and what doesn't 
  dv		[*] = !values.d_nan
  err_dv        [*] = !values.d_nan
  fline         [*] = !values.d_nan
  err_fline     [*] = !values.d_nan
  fcont         [*] = !values.d_nan
  err_fcont     [*] = !values.d_nan
  wc            [*] = !values.d_nan
  err_wc        [*] = !values.d_nan
  ag            [*] = !values.d_nan
  ag_err        [*] = !values.d_nan
  redchisq      [*] = !values.d_nan
  nfree         [*] = !values.d_nan
  numlines      [*] = !values.d_nan
  today         [*] = !values.d_nan
  disp          [*] = !values.d_nan
  refpix        [*] = !values.d_nan
  refwave       [*] = !values.d_nan
  weq           [*] = !values.d_nan
  err_weq       [*] = !values.d_nan
  alf           [*] = !values.d_nan
  err_alf       [*] = !values.d_nan
  intensity     [*] = !values.d_nan
  err_intensity	[*] = !values.d_nan

  CATCH,/CANCEL
  
  ;; This is basically pop_flux and friends.  Use Melanie's variable
  ;; names, more or less, minus the subscripts and made consistant
  ;; with the database names

  this_dir = ''
  for inday = 0, N_ndays-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + string(ndays[inday]), /CONTINUE
     endif else begin

        ;; Read in a file if we need to
        if dirs[inday] ne this_dir then begin
           this_dir = dirs[inday]
           sparinfo_fname = strtrim(dirs[inday], 2) + '/sparinfo_' + $
                            strtrim(round(ndays[inday]), 2) + '.sav'
           restore, sparinfo_fname, /relaxed_structure_assignment
           if N_elements(sparinfo) eq 0 then $
             message, 'ERROR: no saved parinfo found for this entire nday'
           f_idx = where(sparinfo.pfo.status eq !pfo.active, npar)
           if npar eq 0 then $
             message, 'ERROR: no active parameters in this sparinfo set'
        endif ;; Read in a new sparinfo file

        ;; IDL doesn't deal well with the structure in where statements
        test = sparinfo[f_idx].ssg.nday
        our_nday_idx = where(abs(ndays[inday] - test) lt 0.001, $
                             count)
        if count eq 0 then $
          message, 'ERROR: no saved parinfo found for this particular nday'
        ;; unnest
        our_nday_idx = f_idx[our_nday_idx]
        fvers = sparinfo[our_nday_idx].ssg.fver
        ;; --> Assume the best fit is the last one
        best_fver = max(fvers)
        idx = where(sparinfo[our_nday_idx].ssg.fver eq best_fver)
        ;; unnest
        idx = our_nday_idx[idx]
        ;; Get our end markers
        end_idx = where(sparinfo[idx].fixed eq 1 and $
                        sparinfo[idx].pfo.ftype eq 0 and $
                        sparinfo[idx].sso.ptype eq !sso.line, count)
        if count ne 2 then $
          message, 'ERROR: endpoints were not saved with fit'
        ;; unnest
        end_idx = idx[end_idx]
        left_pix = sparinfo[end_idx[0]].value
        right_pix = sparinfo[end_idx[1]].value

        ;; Make sure we have dgs assigned consistently
        sso_dg_assign, sparinfo, idx
        io_dg = sso_path_dg(sso_path_create([!eph.io, !eph.earth]))
        ag_dg = sso_path_dg(sso_path_create([!eph.earth, !eph.earth]))

        ;; Run model to get owls and set up to calculate chisq stuff
        model_spec = pfo_funct(disp_pix, parinfo=sparinfo, $
                               idx=idx, xaxis=wavelengths)
        good_pix = where(finite(disp_pix[*,0]) eq 1 and $
                         finite(wavelengths[*,0]) eq 1 and $
                         finite(spectra[*,0]) eq 1 and $
                         finite(spec_errors[*,0]) eq 1, n_pix)
        if n_pix eq 0 then $
          message, 'ERROR: no good data found for this particular nday.  Hey!  ssg_fit1spec should have complained'

        temp = disp_pix[*,0]
        temp[0:left_pix] = !values.f_nan
        temp[right_pix:N_elements(disp_pix[*,0])-1] = !values.f_nan
        pix_axis = where(finite(temp) eq 1 and $
                         finite(wavelengths[*,0]) eq 1 and $
                         finite(spectra[*,0]) eq 1 and $
                         finite(spec_errors[*,0]) eq 1, n_pix)
        if n_pix eq 0 then $
          message, 'ERROR: no good data found in selected range.  Hey!  ssg_fit1spec should have complained'

        ;; CHISQ
        spec = spectra[pix_axis, 0]
        err_spec = spec_errors[pix_axis, 0]
        model_spec = pfo_funct(pix_axis, parinfo=sparinfo, idx=idx)
        residual = spec - model_spec
        chisq = total((residual/err_spec)^2, /NAN)
        free_idx = where(sparinfo[idx].fixed ne 1 and $
                         sparinfo[idx].pfo.status eq !pfo.active, count)
        nfree[inday] = count
        dof = n_pix - nfree[inday]
        redchisq[inday] = chisq/(dof - 1)

        ;; NUMLINES
        lc_idx = where(sparinfo[idx].sso.ttype eq !sso.center and $
                       sparinfo[idx].sso.ptype eq !sso.line, count)
        if count eq 0 then $
          message, 'ERROR: no lines found'
        numlines[inday] = count

        ;; DATES
        get_date, temp
        today[inday] = temp


        ;; DISPERSION
        disp_idx = where(sparinfo[idx].pfo.inaxis eq !pfo.Xin and $
                         sparinfo[idx].pfo.outaxis eq !pfo.Xaxis, $
                         disp_order)
        if disp_order eq 0 then $
          message, 'ERROR: no dispersion terms found'
        ;; unnest
        disp_idx = idx[disp_idx]
        ftypes = sparinfo[disp_idx].pfo.ftype - !pfo.poly
        prnums = round(ftypes * 100. )
        pridx = where(0 lt prnums and prnums lt 10, count)
        if count ne 1 then $ $
          message, 'ERROR: ' + strtrim(count, 2) + ' reference pixels found.  Old database can only handle 1'
        refpix[inday] = sparinfo[disp_idx[pridx]].value
        ;; get 0th and 1st order coefs.
        cftypes =  ftypes * 1000.
        rcftypes = round(cftypes)
        ;; Pick out the 0th order coefficients and get the polynomial
        ;; numbers from them.
        c0idx = where(0 lt rcftypes and rcftypes lt 10 and $
                      round(cftypes * 10.) eq rcftypes * 10, $
                      npoly)
        if npoly ne 1 then $
          message, 'ERROR: '  + strtrim(npoly, 2) + ' dispersion polynomials found.  Analysis database can only handle 1'
        ;; unnest
        c0idx = disp_idx[c0idx]
        refwave[inday] = sparinfo[c0idx].value
        c1idx = where(0 lt rcftypes and rcftypes lt 10 and $
                      round(cftypes * 10.) eq rcftypes * 10 + 1, $
                      count)
        if count ne 1 then $
          message, 'ERROR: '  + strtrim(count, 2) + ' 1st order dispersion coefs found.  Analysis database can only handle 1'
        ;; unnest
        c1idx = disp_idx[c1idx]
        disp[inday] = sparinfo[c1idx].value / !sso.dwcvt

        ;; IO LINE
        io_idx = where(sparinfo[idx].sso.dg eq io_dg, nio)
        if nio eq 0 then $
          message, 'ERROR: no Io parameters found'
        ;; unnest
        io_idx = idx[io_idx]

        ;; IO EQUIVALENT WIDTH
        ew_idx = where(sparinfo[io_idx].sso.ttype eq !sso.ew, count)
        if count ne 1 then $
          message, 'ERROR: ' + string(count) + ' Io equivalent width parmeters found'
        ;; unnest
        ew_idx = io_idx[ew_idx]
        weq[inday] = sparinfo[ew_idx].value
        err_weq[inday] = sparinfo[ew_idx].error

        ;; CONTINUUM
        cont_idx = where(sparinfo[idx].sso.ptype eq !sso.cont, N_continuum)
        if N_continuum eq 0 then $
          message, 'ERROR: no continuum terms found'
        ;; unnest
        cont_idx = idx[cont_idx]
        fcont[inday] = pfo_funct([sparinfo[ew_idx].sso.owl], $
                                 parinfo=sparinfo, idx=[disp_idx, cont_idx])
        if N_continuum eq 1 then begin
           err_fcont[inday] = sparinfo[cont_idx].error
        endif else begin
           ;; --> fix this, maybe by calculating a bunch of models
           ;; within the error bars + taking the max or something like
           ;; that.
           err_fcont[inday] = 0.
           message, /CONTINUE, 'WARNING: continuum is complicated.  I am arbitrarily setting err_fcont = ' + strtrim(err_fcont[inday], 2) + '.  Please fix this'
        endelse
        fline[inday] = weq[inday] * disp[inday] * !sso.dwcvt * fcont[inday]
        err_fline[inday] = ((err_weq[inday]/weq[inday])^2 + $
                            (err_fcont[inday]/fcont[inday])^2)^(0.5) $
                           * fline[inday]

        ;; IO CONVOLVED LINE WIDTH, wc
        lw_idx = where(sparinfo[io_idx].sso.ttype eq !sso.width, count)
        if count eq 0 then $
          message, 'ERROR: no convolved Io linewidth found'
        ;; unnest
        lw_idx = io_idx[lw_idx]
        for ilw=0, count-1 do begin
           if sparinfo[lw_idx[ilw]].value gt 0 then begin
              if finite(wc[inday]) then begin
                message, /CONTINUE,  'WARNING: too many width parameters for Io line.  Fit should have been done with Lorentzian term fixed at 0'
             endif else begin
                wc[inday] = sparinfo[lw_idx[ilw]].value
                err_wc[inday] = sparinfo[lw_idx[ilw]].error
             endelse ;; Gaussian width
           endif ;; non-zero width
        endfor

        ;; IO DOPPLER SHIFT
        deldot_idx = where(sparinfo[io_idx].sso.ptype eq !sso.dop, count)
        if count ne 1 then $
          message, 'ERROR: ' + strtrim(count, 2) + ' Io Doppler shift parmeters found'
        ;; unnest
        deldot_idx = io_idx[deldot_idx]
        dv[inday] = sparinfo[deldot_idx].value
        err_dv[inday] = sparinfo[deldot_idx].error

        ;; AIRGLOW
        ;; Start assuming no airglow was fit.
        ag[inday] = 0.
        ag_err[inday] = 0.
        ag_idx = where(sparinfo[idx].sso.dg eq ag_dg and $
                       sparinfo[idx].sso.ttype eq !sso.ew and $
                       sparinfo[idx].value gt 0, count)
        if count gt 1 then $
          message, 'ERROR: ' + strtrim(count, 2) + ' airglow equivalent width paramemters found.  This database can only handle 1'
        if count eq 1 then begin
           ;; unnest
           ag_idx = idx[ag_idx]
           ag[inday] = sparinfo[ag_idx].value
           ag_err[inday] = sparinfo[ag_idx].error
        endif

        ;; I think this is the change in V-magnitude starting from the
        ;; sun, bouncing off of Io and ending up at the Earth
        dist_mag = 5*alog10(r[inday]*delta[inday])

        case 1 of
           (phi[inday] ge 355) and (phi[inday] lt 5)   : phi_cor =.04
           (phi[inday] ge 5)   and (phi[inday] lt 15)  : phi_cor =.03
           (phi[inday] ge 15)  and (phi[inday] lt 25)  : phi_cor =  .016
           (phi[inday] ge 25)  and (phi[inday] lt 35)  : phi_cor =.002
           (phi[inday] ge 35)  and (phi[inday] lt 45)  : phi_cor =-.012
           (phi[inday] ge 45)  and (phi[inday] lt 55)  : phi_cor =-.03
           (phi[inday] ge 55)  and (phi[inday] lt 65)  : phi_cor =-.044
           (phi[inday] ge 65)  and (phi[inday] lt 75)  : phi_cor =-.062
           (phi[inday] ge 75)  and (phi[inday] lt 85)  : phi_cor =-.072
           (phi[inday] ge 85)  and (phi[inday] lt 95)  : phi_cor =-.080
           (phi[inday] ge 95)  and (phi[inday] lt 105) : phi_cor =-.074
           (phi[inday] ge 105) and (phi[inday] lt 115) : phi_cor =-.060
           (phi[inday] ge 115) and (phi[inday] lt 125) : phi_cor =-.056
           (phi[inday] ge 125) and (phi[inday] lt 135) : phi_cor =-.048
           (phi[inday] ge 135) and (phi[inday] lt 145) : phi_cor =-.034
           (phi[inday] ge 145) and (phi[inday] lt 155) : phi_cor =-.03
           (phi[inday] ge 155) and (phi[inday] lt 165) : phi_cor =-.028
           (phi[inday] ge 165) and (phi[inday] lt 175) : phi_cor =-.026
           (phi[inday] ge 175) and (phi[inday] lt 185) : phi_cor =-.022
           (phi[inday] ge 185) and (phi[inday] lt 195) : phi_cor =-.020
           (phi[inday] ge 195) and (phi[inday] lt 205) : phi_cor =-.016
           (phi[inday] ge 205) and (phi[inday] lt 215) : phi_cor =-.012
           (phi[inday] ge 215) and (phi[inday] lt 225) : phi_cor =-.01
           (phi[inday] ge 225) and (phi[inday] lt 235) : phi_cor =-.006
           (phi[inday] ge 235) and (phi[inday] lt 245) : phi_cor =-.002
           (phi[inday] ge 245) and (phi[inday] lt 255) : phi_cor =.008
           (phi[inday] ge 255) and (phi[inday] lt 265) : phi_cor =.018
           (phi[inday] ge 265) and (phi[inday] lt 275) : phi_cor =.03
           (phi[inday] ge 275) and (phi[inday] lt 285) : phi_cor =.044
           (phi[inday] ge 285) and (phi[inday] lt 295) : phi_cor =.06
           (phi[inday] ge 295) and (phi[inday] lt 305) : phi_cor =.07
           (phi[inday] ge 305) and (phi[inday] lt 315) : phi_cor =.08
           (phi[inday] ge 315) and (phi[inday] lt 325) : phi_cor =.086
           (phi[inday] ge 325) and (phi[inday] lt 335) : phi_cor =.084
           (phi[inday] ge 335) and (phi[inday] lt 345) : phi_cor =.074
           (phi[inday] ge 345) and (phi[inday] lt 355) : phi_cor =.056
           else: print,'phi has an illegal value'
        endcase  
        if sol_pha[inday] ge 6 then $
          V_cor = -1.55 + DIST_MAG + 0.021*sol_pha[inday] + phi_cor
        if sol_pha[inday] lt 6 then $
          V_cor = -1.7233 +  DIST_MAG + $
          0.078*sol_pha[inday] - 0.0047*(sol_pha[inday])^2 + phi_cor

        exp1= 26 - (20 + 0.4*V_cor)
        nlam= float((1.509 * 3.694 * 10^(exp1))/6300.304 )
        ;; absolute line flux
        alf[inday] = weq[inday] * !sso.ewcvt * nlam
        err_alf[inday] = err_weq[inday] * !sso.ewcvt * nlam

        intensity[inday] = ((alf[inday]*(206265.^2.)*4.)/ $
                            ((1e6)*((io_dia[inday]/2.)^2.)))/1000.
        err_intensity[inday] = (err_alf[inday]*intensity[inday])/alf[inday]

        
     endelse ;; Catching errors

  endfor ;; For each file
  CATCH, /CANCEL

  message, /INFORMATIONAL, 'NOTE: updating analysis database'
  dbopen, adbname, 1
  dbupdate, aentries, $
            'deldot_m,err_deldot_m, fline,err_fline, fcont,err_fcont, wc,err_wc', $
            dv, err_dv, fline, err_fline, fcont, err_fcont, wc, err_wc
  
  dbupdate, aentries, $
            'ag_flux,err_ag_flux, redchisq,freeparam,numlines,db_date,disp,refpix, refwave', $
            ag, ag_err, redchisq, nfree, numlines, today, disp, refpix, refwave
  
  dbupdate, aentries, $
            'weq, err_weq, alf, err_alf, p_date, intensity, err_intensity', $
            weq, err_weq, alf, err_alf, today, intensity, err_intensity
  
  dbclose
  !priv = oldpriv

end

