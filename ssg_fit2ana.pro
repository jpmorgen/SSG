;+
; $Id: ssg_fit2ana.pro,v 1.4 2015/03/04 15:45:43 jpmorgen Exp $

; ssg_fit2ana.  Takes information from the fit database and stuffs it
; into the analysis database

;-

pro ssg_fit2ana, $
   nday_start_or_range, $
   before=before_in, $ ;; UT date or JD before which the maximum fit version will be used
   after=after_in, $ ;; UT date or JD after which the maximum fit version will be used
   interactive=interactive, $ ;; ssg_select date range interactive
   close_lines=close_lines ;; filename for output of close lines database

  init = {ssg_sysvar}
  init = {tok_sysvar}

;  ON_ERROR, 2
  oldpriv=!priv
  !priv = 2
  CATCH, err
  if err ne 0 then begin
     message, /NONAME, !error_state.msg, /CONTINUE
     message, 'Closing database(s) and exiting gracefully',/CONTINUE
     dbclose
     !priv = oldpriv
     return
  endif

  if NOT keyword_set(nday_start_or_range) then nday_start_or_range=0

  ;; Default to the latest fit, which is hopefully guaranteed to be
  ;; before now
  before = systime(/Julian, /UT)
  ;; Check to see if an earlier date is specified
  if N_elements(before_in) ne 0 then begin
     ;; Assume date is in JD
     before = before_in
     ;; Check to see if it is in string form of YYYY-MM-DDTHH:MM:SS
     if size(/type, before) eq !tok.string then begin
        datearr=strsplit(before,'-T:',/extract)
        ;; juldate returns reduced Julian Day
        juldate, double(datearr), before
        before += !eph.jd_reduced
     endif
  endif
  after = 0
  ;; Check to see if a later date is specified
  if N_elements(after_in) ne 0 then begin
     ;; Assume date is in JD
     after = after_in
     ;; Check to see if it is in string form of YYYY-MM-DDTHH:MM:SS
     if size(/type, after) eq !tok.string then begin
        datearr=strsplit(after,'-T:',/extract)
        ;; juldate returns reduced Julian Day
        juldate, double(datearr), after
        after += !eph.jd_reduced
     endif
  endif
  

  c = 299792.458 ;; km/s

  if keyword_set(interactive) then $
     message, /CONTINUE, 'Select range of observations over which to extract Io intensity measurements'
  ;; Just use ssg_select to get range of ndays
  ndays=ssg_select(nday_start_or_range, count=count, $
                   title='Select spectra to transfer to analysis database', $
                   non_interactive=(NOT keyword_set(interactive)))
  if count eq 0 then return

  nday_range = minmax(ndays)
  rdbname = 'ssg_reduce'
;  fdbname = 'oi_6300_fit'
;  adbname = 'io6300_integrated'
  adbname = 'io_oi_analyze'
  dbopen, adbname, 0
  begin_nday = nday_range[0]
  end_nday = nday_range[1]
  aentries = dbfind(string("nday>", begin_nday), $
                    dbfind(string("nday<", end_nday)), count=adays)
  if adays eq 0 then begin
     message, /CONTINUE, 'No entries found in analysis database'
     return
  endif
  ;; Make the analysis database our master for ndays
  dbext, aentries, 'nday, object, obj_code', ndays, objects, obj_codes
  ;; Collect things we need for our calculations
  dbext, aentries, 'delta,r,phi,spa,ang_dia', delta, r, phi, sol_pha, obj_dia
  ;; Collect arrays for our output results
  dbext, aentries, $
         'deldot_m,err_deldot_m, fline,err_fline, fcont,err_fcont, wc,err_wc', $
         dv, err_dv, fline, err_fline, fcont, err_fcont, wc, err_wc

  dbext, aentries, $
         'ag_flux,err_ag_flux, ag_wc, err_ag_wc', $
         ag, ag_err, ag_wc, err_ag_wc

  dbext, aentries, $
         'redchisq,freeparam,numlines,ip,db_date,disp,refpix, refwave', $
         redchisq, nfree, numlines, ips, today, disp, refpix, refwave

  dbext, aentries, $
         'weq, err_weq, alf, err_alf, p_date, intensity, err_intensity', $
         weq, err_weq, alf, err_alf, today, intensity, err_intensity

  dbext, aentries, $
         'nn_DOWL, nn_ew, nn_Gw, nn_Lw, n2_DOWL, n2_ew, n2_Gw, n2_Lw', $
         nn_DOWLs, nn_ews, nn_Gws, nn_Lws, n2_DOWLs, n2_ews, n2_Gws, n2_Lws

  dbclose

  ;; Possibly phasing out the fit database in favor of parinfo.sav files
  dbopen, rdbname
  entries = where_nday_eq(ndays, count=N_ndays)

  if N_ndays ne adays then $
     message, 'ERROR: adb and fitdb nday missmatch'

  dbext, entries, 'dir, disp_pix, spectrum, spec_err', $
         dirs, disp_pix, spectra, spec_errors
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
  ag_wc         [*] = !values.d_nan
  err_ag_wc     [*] = !values.d_nan
  redchisq      [*] = !values.d_nan
  nfree         [*] = !values.d_nan
  numlines      [*] = !values.d_nan
  ips		[*] = !values.d_nan
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
  
  ;; If we want, save off a structure with our close lines calculations
  if keyword_set(close_lines) then begin
     if size(/type, close_lines) ne !tok.string then $
        message, 'ERROR: close_lines keyword must be a string indicating a .sav file to store solar line array in'
     ;; Create a single parinfo element with the right tags to store
     ;; our analysis stuff.  For now, just make it a regular
     ;; ssg_parinfo plus ssg_ana_struct.  I could pare it down if I
     ;; wanted
  endif ;; recording close lines
  ;; Whether or not we record, we work with the close lines
  ssg_ana_struct__define, parinfo=ssg_ana_parinfo1
  N_close_lines = N_elements(ssg_ana_parinfo1.sso_ana.RWL)
  
  CATCH,/CANCEL
  
  ;; This is basically pop_flux and friends.  Use Melanie's variable
  ;; names, more or less, minus the subscripts and made consistant
  ;; with the database names

  this_dir = ''
  for inday = 0, adays-1 do begin
     CATCH, err
     if err ne 0 then begin
        CATCH, /CANCEL
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + string(ndays[inday]), /CONTINUE
        CONTINUE
     endif

     ;; Read in a file if we need to
     if dirs[inday] ne this_dir then begin
        this_dir = dirs[inday]
        sparinfo_fname = strtrim(dirs[inday], 2) + '/sparinfo_' + $
                         strtrim(floor(ndays[inday]), 2) + '.sav'
        restore, sparinfo_fname, /relaxed_structure_assignment
        if N_elements(sparinfo) eq 0 then begin
           message, 'WARNING: no saved parinfo found for this entire nday', /CONTINUE
           CONTINUE
        endif
        f_idx = where(sparinfo.pfo.status eq !pfo.active, npar)
        if npar eq 0 then $
           message, 'ERROR: no active parameters in this sparinfo set'
     endif ;; Read in a new sparinfo file

     ;; Get in the indices into our nday
     ;; IDL doesn't deal well with the structure in where statements
     test = sparinfo[f_idx].ssg.nday
     our_nday_idx = where(abs(ndays[inday] - test) lt !ssg.tolerance, $
                          count)
     if count eq 0 then $
        message, 'ERROR: no saved parinfo found for this particular nday'
     ;; unwrap
     our_nday_idx = f_idx[our_nday_idx]

     ;; Handle the before keyword so that we can go backward in our
     ;; analysis if we need to
     fdate = sparinfo[our_nday_idx].ssg.fdate
     ;; Handle the case of before=0 separately, since that is our
     ;; first fit before we had .fdate and easy to get to by saying
     ;; before=0.  For subsequent calls, before might be the actual
     ;; first fdate of the run you want to exclude.
     if before eq 0 then begin
        before_idx = where(fdate eq 0, count)
     endif else begin
        before_idx = where(fdate lt before, count)
     endelse
     if count eq 0 then begin
        caldat, before, month, day, year, hour, minute, second
        datestr = string(format='(I4, 2("-", I02), "T", 3(I02, :, ":") )', year, month, day, hour, minute, second)
        message, 'ERROR: no parinfo were saved before ' + datestr
     endif
     ;; unwrap back into our_nday_idx
     our_nday_idx = our_nday_idx[before_idx]

     ;; Handle our after keyword so that we can exclude earlier
     ;; results
     if after gt 0 then begin
        after_idx = where(fdate[our_nday_idx] gt after, count)
        if count eq 0 then begin
           caldat, after, month, day, year, hour, minute, second
           datestr = string(format='(I4, 2("-", I02), "T", 3(I02, :, ":") )', year, month, day, hour, minute, second)
           message, 'ERROR: no parinfo were saved after ' + datestr
        endif
        ;; unwrap back into our_nday_idx
        our_nday_idx = our_nday_idx[after_idx]
     endif

     ;; Assume the best fit is the last one
     fvers = sparinfo[our_nday_idx].ssg.fver
     best_fver = max(fvers)
     idx = where(sparinfo[our_nday_idx].ssg.fver eq best_fver)
     ;; unwrap
     idx = our_nday_idx[idx]
     ;; Get our end markers
     end_idx = where(sparinfo[idx].fixed eq 1 and $
                     sparinfo[idx].pfo.ftype eq 0 and $
                     sparinfo[idx].sso.ptype eq !sso.line, count)
     if count ne 2 then $
        message, 'ERROR: endpoints were not saved with fit'
     ;; unwrap
     end_idx = idx[end_idx]
     left_pix = sparinfo[end_idx[0]].value
     right_pix = sparinfo[end_idx[1]].value

     ;; Make sure we have dgs assigned consistently
     sso_dg_assign, sparinfo, idx
     obj_dg = sso_path_dg(sso_path_create([!eph.jupiter - 99 + obj_codes[inday], !eph.earth]))
     earth_dg = sso_path_dg(sso_path_create([!eph.earth, !eph.earth])) ;; this includes telluric absorption too

     ;; Run model to get owls and set up to calculate chisq stuff
     model_spec = pfo_funct(disp_pix[*,inday], parinfo=sparinfo, $
                            idx=idx, xaxis=wavelengths)
     good_pix = where(finite(disp_pix[*,inday]) eq 1 and $
                      finite(wavelengths) eq 1 and $
                      finite(spectra[*,inday]) eq 1 and $
                      finite(spec_errors[*,inday]) eq 1, n_pix)
     if n_pix eq 0 then $
        message, 'ERROR: no good data found for this particular nday.  Hey!  ssg_fit1spec should have complained'

     temp = disp_pix[*,0]
     temp[0:left_pix] = !values.f_nan
     temp[right_pix:N_elements(disp_pix[*,inday])-1] = !values.f_nan
     pix_axis = where(finite(temp) eq 1 and $
                      finite(wavelengths) eq 1 and $
                      finite(spectra[*,inday]) eq 1 and $
                      finite(spec_errors[*,inday]) eq 1, n_pix)
     if n_pix eq 0 then $
        message, 'ERROR: no good data found in selected range.  Hey!  ssg_fit1spec should have complained'

     ;; CHISQ
     spec = spectra[pix_axis, inday]
     err_spec = spec_errors[pix_axis, inday]
     ;; Replace the wavelengths axis for calculations below
     model_spec = pfo_funct(pix_axis, parinfo=sparinfo, idx=idx, xaxis=wavelengths)
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
     ;; unwrap
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
     ;; unwrap
     c0idx = disp_idx[c0idx]
     refwave[inday] = sparinfo[c0idx].value
     c1idx = where(0 lt rcftypes and rcftypes lt 10 and $
                   round(cftypes * 10.) eq rcftypes * 10 + 1, $
                   count)
     if count ne 1 then $
        message, 'ERROR: '  + strtrim(count, 2) + ' 1st order dispersion coefs found.  Analysis database can only handle 1'
     ;; unwrap
     c1idx = disp_idx[c1idx]
     disp[inday] = sparinfo[c1idx].value / !sso.dwcvt

     ;; CLOSE LINES
     ;; Thu Jul 30 14:39:32 2015  jpmorgen@snipe
     ;; Fill up an ssg_ana_parinfo with information on the parameters of
     ;; the <N_close_lines> closest lines to each line.  We have
     ;; already checked to make sure that there are lines found up in
     ;; the NUMLINES code
     ;; We want to fill in an ssg_ana_parinfo.  Make it a duplicate
     ;; of our particular segment of sparinfo, we will eventually
     ;; concatenate the ssg_ana_parinfo into an sssg_ana_parinfo for
     ;; saving on disk
     ;; At this point, idx points to our fit, including endpoints
     ssg_ana_parinfo = replicate(ssg_ana_parinfo1, N_elements(idx))
     struct_assign, sparinfo[idx], ssg_ana_parinfo, /verbose
     ;; Get our lc_idx again, since we have a subset of the
     ;; original sparinfo
     lc_idx = where(ssg_ana_parinfo.sso.ttype eq !sso.center and $
                    ssg_ana_parinfo.sso.ptype eq !sso.line)

     for iline=0, numlines[inday]-1 do begin
        ;; Delta observed wavelength = close line minus host line
        ;; gets delta observed wavelegnth lined up sensibly in
        ;; ssg_ana_close_lines.  Convert to our display units here
        dowl = (ssg_ana_parinfo[lc_idx].sso.owl - $
               ssg_ana_parinfo[lc_idx[iline]].sso.owl) / !sso.dwcvt
        err_dowl = sqrt(ssg_ana_parinfo[lc_idx].error^2 + $
                        ssg_ana_parinfo[lc_idx[iline]].error^2) / !sso.dwcvt
        ;; Sort DOWL by the absolute value, so we get our true
        ;; closest lines
        dowl_sort_idx = sort(abs(dowl))
        ;; Before we unwrap, save DOWL and err_DOWL in the line
        ;; center parameter.  The 0th line is always the line
        ;; itself
        ssg_ana_parinfo[lc_idx[iline]].sso_ana.DOWL = $
           dowl[dowl_sort_idx[1:N_close_lines]]
        ssg_ana_parinfo[lc_idx[iline]].sso_ana.err_DOWL = $
           err_dowl[dowl_sort_idx[1:N_close_lines]]
        
        ;; Unwrap, dropping off the 0th idx, since that is the line itself
        dowl_sort_idx = lc_idx[dowl_sort_idx[1:N_close_lines]]

        ;; Now we can get RWLs, dgs, and paths of our close lines
        ssg_ana_parinfo[lc_idx[iline]].sso_ana.RWL = $
           ssg_ana_parinfo[dowl_sort_idx].sso.RWL
        ssg_ana_parinfo[lc_idx[iline]].sso_ana.dg = $
           ssg_ana_parinfo[dowl_sort_idx].sso.dg
        ;; I don't think IDL would get the implicit array
        ;; dimensions right, so copy paths explicitly, close line
        ;; by close line
        for icline=0, N_close_lines-1 do begin
           ssg_ana_parinfo[lc_idx[iline]].sso_ana.path[icline,*] = $
              ssg_ana_parinfo[dowl_sort_idx[icline]].sso.path
        endfor ;; copy path for each close line

        ;; Get indices into all parameters of our iline
        iline_idx = where(ssg_ana_parinfo.sso.RWL eq $
                          ssg_ana_parinfo[lc_idx[iline]].sso.RWL and $
                          ssg_ana_parinfo.sso.dg eq $
                          ssg_ana_parinfo[lc_idx[iline]].sso.dg, npar)
        if npar eq !pfo.fnpars[!pfo.voigt] and floor(ssg_ana_parinfo[lc_idx[iline]].sso.pfo.pfo.ftype) ne !pfo.voigt then $
           message, 'ERROR: I really only know how to deal with Voigts right now'

        ;; Pull up the Doppler shift our this line and calculate
        ;; how far it is off from the expected value
        iline_dop_idx = where(ssg_ana_parinfo.sso.ptype eq !sso.dop and $
                              ssg_ana_parinfo.sso.dg eq ssg_ana_parinfo[lc_idx[iline]].sso.dg, ndop)
        if ndop ne 1 then $
           message, 'ERROR: expected Doppler parameter for primary line not found for this spectrum'

        iline_eph_deldot = sso_eph_dop(nday2date(ndays[inday]), $
                                       ssg_ana_parinfo[iline_dop_idx].sso.path, $
                                       !ssg.mmp_xyz)
        v2c = !ssg.c / ssg_ana_parinfo[lc_idx[iline]].sso.OWL
        delta_dop = (ssg_ana_parinfo[iline_dop_idx].value - iline_eph_deldot) $
                    / v2c / !sso.dwcvt ;; convert from real wavelength to display wavelength
        
        err_delta_dop = sqrt((ssg_ana_parinfo[iline_dop_idx].error^2 $
                              + ssg_ana_parinfo[iline_dop_idx].error^2) $
                             / v2c^2)

        ;; Now we need to move to the other parameters.  Use RWL
        ;; and DG as a handle to pull up all the parametes of a
        ;; particular line
        for icline=0, N_close_lines-1 do begin
           clidx = where(ssg_ana_parinfo.sso.RWL eq $
                         ssg_ana_parinfo[lc_idx[iline]].sso_ana.RWL[icline] and $
                         ssg_ana_parinfo.sso.dg eq $
                         ssg_ana_parinfo[lc_idx[iline]].sso_ana.dg[icline], count)
           if count eq 0 then $
              message, 'ERROR: not able to pull up line parameters by RWL/dg.  Something is really wrong.'
           ;; I am fairly confident that all lines are Voigts and
           ;; the parameters are in order, so I don't need
           ;; to write general code, but check to make sure

           bad_idx = where(ssg_ana_parinfo[iline_idx].pfo.ftype ne $
                           ssg_ana_parinfo[clidx].pfo.ftype, count)
           if count ne 0 then $
              message, 'ERROR: Line ftype mismatch.  It is going to take some additional coding to line up the parameters'
           ;; Now just assume everything is a Voigt.  Stash the
           ;; reduced chi2 of our fit and the delta Doppler shift
           ;; and error from expected of our host line.  Get rid of
           ;; the trivial dimension for the *delta_dop tags
           ssg_ana_parinfo[lc_idx[iline]:lc_idx[iline]+3].sso_ana.redchisq = redchisq[inday]
           ssg_ana_parinfo[lc_idx[iline]:lc_idx[iline]+3].sso_ana.delta_dop = delta_dop[0]
           ssg_ana_parinfo[lc_idx[iline]:lc_idx[iline]+3].sso_ana.err_delta_dop = err_delta_dop[0]
           ;; Copy over RWL, dg, DOWL, and err_DOWL from the line
           ;; center to each close line
           ssg_ana_parinfo[lc_idx[iline]+1:lc_idx[iline]+3].sso_ana.RWL[icline] = $
              ssg_ana_parinfo[lc_idx[iline]].sso_ana.RWL[icline]
           ssg_ana_parinfo[lc_idx[iline]+1:lc_idx[iline]+3].sso_ana.dg[icline] = $
              ssg_ana_parinfo[lc_idx[iline]].sso_ana.dg[icline]
           ssg_ana_parinfo[lc_idx[iline]+1:lc_idx[iline]+3].sso_ana.DOWL[icline] = $
              ssg_ana_parinfo[lc_idx[iline]].sso_ana.DOWL[icline]
           ssg_ana_parinfo[lc_idx[iline]+1:lc_idx[iline]+3].sso_ana.err_DOWL[icline] = $
              ssg_ana_parinfo[lc_idx[iline]].sso_ana.err_DOWL[icline]
           ;; Copy the value and error for the close lines into the
           ;; structure
           ssg_ana_parinfo[lc_idx[iline]:lc_idx[iline]+3].sso_ana.value[icline] = $
              ssg_ana_parinfo[clidx].value
           ssg_ana_parinfo[lc_idx[iline]:lc_idx[iline]+3].sso_ana.error[icline] = $
              ssg_ana_parinfo[clidx].error
           ;; path requires a little more work to copy from the
           ;; line center parameter, because of implicit array
           ;; index confusion
           for ipar=1,!pfo.fnpars[!pfo.voigt]-1 do begin
              ssg_ana_parinfo[lc_idx[iline]+ipar].sso_ana.path[icline,*] = $
                 ssg_ana_parinfo[lc_idx[iline]].sso.path
           endfor ;; each parameter for path
        endfor    ;; handle parameters for each close line           
     endfor       ;; each line
     ;; Accumulate close lines for saving as we exit (if filename
     ;; supplied)
     if keyword_set(close_lines) then $
        sssg_ana_parinfo = array_append(ssg_ana_parinfo, sssg_ana_parinfo)

     ;; CONTINUUM
     cont_idx = where(sparinfo[idx].sso.ptype eq !sso.cont, N_continuum)
     if N_continuum eq 0 then $
        message, 'ERROR: no continuum terms found'
     ;; unwrap
     cont_idx = idx[cont_idx]
     ;; Pass our reference pixel into pfo_funct using just dispersion and
     ;; continuum to get our continuum value at the reference pixel.
     ;; This will be improved to be at the object line, below, if we
     ;; have one
     fcont[inday] = pfo_funct([refpix[inday]], $
                              parinfo=sparinfo, idx=[disp_idx, cont_idx])
     ;; And convert from per pixel to per angstrom (convert disp back
     ;; to A from mA)
     fcont[inday] /= disp[inday] * !sso.dwcvt
     ;; Thu Sep 17 17:22:24 2015  jpmorgen@snipe
     ;; For now, just use the median data error bar for the continuum
     ;; error, since that is the fiducial I want anyway
     ;; --> CCD gain might be off, and thus continuum error bars,
     ;; since there is a wide range of continuum values
     err_fcont[inday] = median(spec_errors)
     err_fcont[inday] /= disp[inday] * !sso.dwcvt
     
     ;; if N_continuum eq 1 then begin
     ;;    err_fcont[inday] = sparinfo[cont_idx].error
     ;; endif else begin
     ;;    ;; --> fix this, maybe by calculating a bunch of models
     ;;    ;; within the error bars + taking the max or something like
     ;;    ;; that.
     ;;    err_fcont[inday] = 0.
     ;;    message, /CONTINUE, 'WARNING: continuum is complicated.  I am arbitrarily setting err_fcont = ' + strtrim(err_fcont[inday], 2) + '.  Please fix this'
     ;; endelse

     ;; AIRGLOW
     ;; Start assuming no airglow was fit.  I think I like having NaN
     ;; be the answer in this case
     ;;ag[inday] = 0.
     ;;ag_err[inday] = 0.
     ag_idx = where(sparinfo[idx].sso.dg eq earth_dg and $
                    sparinfo[idx].sso.ttype eq !sso.ew and $
                    sparinfo[idx].value gt 0, ag_count)
     if ag_count gt 1 then $
        message, 'ERROR: ' + strtrim(count, 2) + ' airglow equivalent width paramemters found.  This database can only handle 1'
     if ag_count eq 1 then begin
        ;; unwrap
        ag_idx = idx[ag_idx]
        ag[inday] = sparinfo[ag_idx].value
        ag_err[inday] = sparinfo[ag_idx].error
        ag_wc[inday] = sparinfo[ag_idx+1].value
        err_ag_wc[inday] = sparinfo[ag_idx+1].error
     endif
     
     ;; IP instrument profile.  Estimate form the Gaussian width of the
     ;; telluric lines
     earth_ew_idx = where(sparinfo[idx].sso.dg eq earth_dg and $
                          sparinfo[idx].sso.ttype eq !sso.ew and $
                          sparinfo[idx].value lt 0, earth_count)
     ;; Unlikely to have zero telluric lines, but check just in case
     ;; and don't raise an error if it is so
     if earth_count gt 0 then begin
        ;; Like ssg_ana_close_lines, cheat and assume all our Voigt
        ;; parameters are lined up, so to get to the Gw, add 1 to the
        ;; ew IDX.
        ;; unwrap
        earth_ew_idx = idx[earth_ew_idx]
        ;; Assume we only have one
        ips[inday] = sparinfo[earth_ew_idx[0] + 1].value
        ;; Take median if we have more than one
        if earth_count gt 1 then $
           ips[inday] = median(sparinfo[earth_ew_idx + 1].value)
     endif

     ;; OBJECT LINE
     obj_idx = where(sparinfo[idx].sso.dg eq obj_dg, nobj)
     if nobj eq 0 then $
        message, 'ERROR: no object parameters found'
     ;; unwrap
     obj_idx = idx[obj_idx]

     ;; OBJECT EQUIVALENT WIDTH
     ew_idx = where(sparinfo[obj_idx].sso.ttype eq !sso.ew, count)
     if count ne 1 then $
        message, 'ERROR: ' + string(count) + ' equivalent width parmeters found for ' + objects[inday]
     ;; unwrap
     ew_idx = obj_idx[ew_idx]
     weq[inday] = sparinfo[ew_idx].value
     err_weq[inday] = sparinfo[ew_idx].error

     ;; If we made it here, we can improve our continuum value from
     ;; the reference pixel to the object line
     obj_pix = interpol(pix_axis, wavelengths, sparinfo[ew_idx-1].sso.owl)
     ;; Pass that pixel into pfo_funct using just dispersion and
     ;; continuum to get our continuum value at the object line
     fcont[inday] = pfo_funct([obj_pix], $
                              parinfo=sparinfo, idx=[disp_idx, cont_idx])
     ;; And convert from per pixel to per angstrom (convert disp back
     ;; to A from mA)
     fcont[inday] /= disp[inday] * !sso.dwcvt

     ;; OBJECT CONVOLVED LINE WIDTH, wc
     lw_idx = where(sparinfo[obj_idx].sso.ttype eq !sso.width, count)
     if count eq 0 then $
        message, 'ERROR: no convolved object linewidth found'
     ;; unwrap
     lw_idx = obj_idx[lw_idx]
     for ilw=0, count-1 do begin
        if sparinfo[lw_idx[ilw]].value gt 0 then begin
           if finite(wc[inday]) then begin
              message, /CONTINUE,  'WARNING: too many width parameters for Io line.  Fit should have been done with Lorentzian term fixed at 0'
           endif else begin
              wc[inday] = sparinfo[lw_idx[ilw]].value
              err_wc[inday] = sparinfo[lw_idx[ilw]].error
           endelse ;; Gaussian width
        endif      ;; non-zero width
     endfor

     ;; OBJECT FLUX 
     fline[inday] = weq[inday] * disp[inday] * !sso.dwcvt * fcont[inday]
     err_fline[inday] = ((err_weq[inday]/weq[inday])^2 + $
                         (err_fcont[inday]/fcont[inday])^2)^(0.5) $
                        * fline[inday]

     ;; OBJECTS DOPPLER SHIFT
     deldot_idx = where(sparinfo[obj_idx].sso.ptype eq !sso.dop, count)
     if count ne 1 then $
        message, 'ERROR: ' + strtrim(count, 2) + ' Object Doppler shift parmeters found'
     ;; unwrap
     deldot_idx = obj_idx[deldot_idx]
     dv[inday] = sparinfo[deldot_idx].value
     err_dv[inday] = sparinfo[deldot_idx].error

     ;; This is the change in magnitude starting from the sun,
     ;; bouncing off of Io and ending up at the Earth assuming a
     ;; perfect reflection
     dist_mag = 5*alog10(r[inday]*delta[inday])

     ;; I think this is from Jason Corliss' thesis, where he
     ;; looked at Galileo observation of Io.  --> There is an old reference
     ;; somewhere of Io's brightness to which this can be
     ;; compared.  Make default no correction.  Correction will be for
     ;; Io only at this point
     phi_cor = 0.
     case obj_codes[inday] eq 1 of
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
        else: message, 'ERROR: phi has an illegal value'
     endcase
     ;; Correct for distance magnitude and phi correction
     V_cor = DIST_MAG + phi_cor
     ;; This seems to be correcting for albedo vs. solar phase angle
     ;; (opposition effect) --> This needs to be a case statement for
     ;; the albedos of the other satellites, assuming any emission is
     ;; found on them.
     if sol_pha[inday] ge 6 then $
        V_cor += -1.55 + 0.021*sol_pha[inday]
     if sol_pha[inday] lt 6 then $
        V_cor += -1.7233 + $
                0.078*sol_pha[inday] - 0.0047*(sol_pha[inday])^2

     exp1= 26 - (20 + 0.4*V_cor)
     nlam= float((1.509 * 3.694 * 10^(exp1))/6300.304 )
     ;; apparent line flux photons/sec/cm^2
     alf[inday] = weq[inday] * !sso.ewcvt * nlam
     err_alf[inday] = err_weq[inday] * !sso.ewcvt * nlam

     ;; Convert to kR
     intensity[inday] = ((alf[inday]*(206265.^2.)*4.)/ $
                         ((1e6)*((obj_dia[inday]/2.)^2.)))/1000.
     err_intensity[inday] = (err_alf[inday]*intensity[inday])/alf[inday]

     ;; Calculate the nearest neighbor stuff for our object line.  If
     ;; we made it here, we have an object line.  Find it in our
     ;; ssg_ana_parinfo
     obj_lc_idx = where(ssg_ana_parinfo.sso.dg eq obj_dg and $
                        ssg_ana_parinfo.sso.ttype eq !sso.center and $
                        ssg_ana_parinfo.sso.ptype eq !sso.line, nobj)
     if nobj eq 0 then $
        message, 'ERROR: no object parameters found in ssg_ana_parinfo.  I just put them here, so I don''t expect this!'
     nn_DOWLs[inday] = ssg_ana_parinfo[obj_lc_idx].sso_ana.DOWL[0]
     nn_ews  [inday] = ssg_ana_parinfo[obj_lc_idx+1].sso_ana.value[0]
     nn_Gws  [inday] = ssg_ana_parinfo[obj_lc_idx+2].sso_ana.value[0]
     nn_Lws  [inday] = ssg_ana_parinfo[obj_lc_idx+3].sso_ana.value[0]
     n2_DOWLs[inday] = ssg_ana_parinfo[obj_lc_idx].sso_ana.DOWL[1]
     n2_ews  [inday] = ssg_ana_parinfo[obj_lc_idx+1].sso_ana.value[1]
     n2_Gws  [inday] = ssg_ana_parinfo[obj_lc_idx+2].sso_ana.value[1]
     n2_Lws  [inday] = ssg_ana_parinfo[obj_lc_idx+3].sso_ana.value[1]

  endfor   ;; for each nday
  CATCH, /CANCEL

  message, /INFORMATIONAL, 'NOTE: updating analysis database'
  dbopen, adbname, 1
  dbupdate, aentries, $
            'deldot_m,err_deldot_m, fline,err_fline, fcont,err_fcont, wc,err_wc', $
            dv, err_dv, fline, err_fline, fcont, err_fcont, wc, err_wc

  dbupdate, aentries, $
         'redchisq,freeparam,numlines,ip,db_date,disp,refpix, refwave', $
         redchisq, nfree, numlines, ips, today, disp, refpix, refwave

  dbupdate, aentries, $
         'ag_flux,err_ag_flux, ag_wc, err_ag_wc', $
         ag, ag_err, ag_wc, err_ag_wc
  dbupdate, aentries, $
            'weq, err_weq, alf, err_alf, p_date, intensity, err_intensity', $
            weq, err_weq, alf, err_alf, today, intensity, err_intensity

  dbupdate, aentries, $
            'nn_DOWL, nn_ew, nn_Gw, nn_Lw, n2_DOWL, n2_ew, n2_Gw, n2_Lw', $
            nn_DOWLs, nn_ews, nn_Gws, nn_Lws, n2_DOWLs, n2_ews, n2_Gws, n2_Lws

  dbclose
  !priv = oldpriv

  ;; Write ssg_ana_parinfo
  if keyword_set(close_lines) then begin
     message, /INFORMATIONAL, 'NOTE: saving solar lines in ' + close_lines
     save, sssg_ana_parinfo, filename=close_lines
  endif

end

