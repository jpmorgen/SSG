;+
; $Id: ssg_fit2ana.pro,v 1.1 2002/12/16 13:35:20 jpmorgen Exp $

; ssg_fit2ana.  Takes information from the fit database and stuffs it
; into the analysis database

;-

pro ssg_fit2ana

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

  get_date,today

  message, /CONTINUE, 'Select observations from which to extract an Io intensity measurements'
  ndays=ssg_select(count=count, /multi, title='Select spectra to transfer to analysis database')
  if count eq 0 then return
  fdbname = 'oi_6300_fit'
  adbname = 'io6300_integrated'
  dbopen, adbname, 0
  aentries = where_nday_eq(ndays, count=adays)
   if count ne adays then begin
     message, 'WARNING: fitting and analysis databases are not lining up properly, using the analysis as the master', /CONTINUE
     dbext, aentries, 'nday', ndays
  endif

  dbopen, fdbname, 0
  fentries = where_nday_eq(ndays, count=N_ndays, tolerance=0.005)
  if N_ndays ne adays then $
    message, 'ERROR: internal database weirdness'

  CATCH,/CANCEL
  for i = 0, N_ndays-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + string(ndays[i]), /CONTINUE
     endif else begin
        dbopen, fdbname, 0
        dbext, fentries[i], 'value, perror, llimited, rlimited, llimits, rlimits, vfID, ssgID, ssggroupID, ssgrwl, ssgowl, ssgdop', $
          dvalues, $
          ddbperrors, $
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
        dbext, fentries[i], 'nfree, chisq, redchisq', nfree, chisq, redchisq
        dbclose

        values = float(dvalues)
        dbperrors = float(ddbperrors)
        cont_idx = where(ssgID eq ssgid_cont, count)
        if count eq 0 then $
          message, 'ERROR: no continuum terms found'
        deldot_idx = where(ssgID eq ssgid_dop and ssgdop eq id_Io_dop, count)
        if count ne 1 then $
          message, 'ERROR: ' + count + ' Io doppler shift parmeters found'
        Io_line_idx = where(ssggroupID eq 1, count)
        if count ne 4 then $
          message, 'ERROR: ' + count + ' Io peak parameters found instead of 4'
        ag = 0.
        ag_err = 0.
        ag_idx = where(ssggroupID eq 5 and ssgrwl eq 6300.304d, count)
        if count eq 1 then begin
           ag = values[ag_idx[vfid_area-2]]
           ag_err = dbperrors[ag_idx[vfid_area-2]]
        endif
        
        dbopen, adbname, 1
        entry = where_nday_eq(ndays[i], $
                              COUNT=count,SILENT=silent, tolerance=0.005) 
        if count eq 0 then $
          message, 'ERROR: no entry for nday ' + string(ndays[i]) + ' in ' + adbname

        fline = values[Io_line_idx[vfid_area-2]]
        err_fline = dbperrors[Io_line_idx[vfid_area-2]]
        dv = values[deldot_idx]
        err_dv = dbperrors[deldot_idx]
        ;; Calculate the continuum value at the observed wavelength of
        ;; the Io line
        owl = 6300.304d * ( 1. + dv / c )
        fcont = float(contspec(owl, values[cont_idx]))
        ;; --> FIX THIS along with the continuum stuff
        err_fcont = 0.

        dbupdate, entry, 'deldot_m, err_deldot_m, fline, err_fline, fcont, err_fcont, wc, err_wc', $
          dv, err_dv, fline, err_fline, fcont, err_fcont, $
          values[Io_line_idx[vfid_dop-2]], dbperrors[Io_line_idx[vfid_dop-2]]

; wd                
; err_wd            
; weq               see below
; err_weq           see below
; alf               see below
; err_alf           see below
        line_idx = where(ssgID ge ssgid_voigt and ssgrwl ne 0, numlines)
        disp_idx = where(ssgID eq ssgid_disp, count)
        if count eq 0 then $
          message, 'ERROR: no dispersion terms found'


        dbupdate, entry, 'ag_flux, err_ag_flux, redchisq, freeparam, numlines, db_date, disp, refpix, refwave', $
          ag, ag_err, float(redchisq), nfree, fix(numlines), today, $
          values[disp_idx[1]], 400., values[disp_idx[0]]

;comp_min          
;comp_max          
;vers              
;db_date           

        ;; --> For lack of a better place, I am going to put these
        ;; things here.  This is basically pop_flux and friends.  Use
        ;; Melanie's variable names, more or less, minus the
        ;; subscripts an made consistant with the database names

        weq = fline/fcont
        err_weq = (((err_fline/fline)^2 + $
                   (err_fcont/fcont)^2)^(0.5)) * weq

        ;; --> let's hope these are there!
        dbext, entry, 'delta,r,phi,spa,io_dia', $
          delta, r, aphi, sol_pha, io_dia
;          adelta, ar, aphi, asol_pha, aio_dia

;          delta   = adelta  [0]
;          r       = ar      [0]
          phi     = aphi    [0]
;          sol_pha = asol_pha[0]
;          io_dia  = aio_dia [0]

        ;; I think this is the change in V-magnitude starting from the
        ;; sun, bouncing off of Io and ending up at the Earth
        dist_mag = 5*alog10(r*delta)

        case 1 of
           (phi ge 355) and (phi lt 5) : phi_cor =.04
           (phi ge 5) and (phi lt 15) : phi_cor =.03
           (phi ge 15) and (phi lt 25) : phi_cor =  .016
           (phi ge 25) and (phi lt 35) : phi_cor =.002
           (phi ge 35) and (phi lt 45) : phi_cor =-.012
           (phi ge 45) and (phi lt 55) : phi_cor =-.03
           (phi ge 55) and (phi lt 65) : phi_cor =-.044
           (phi ge 65) and (phi lt 75) : phi_cor =-.062
           (phi ge 75) and (phi lt 85) : phi_cor =-.072
           (phi ge 85) and (phi lt 95) : phi_cor =-.080
           (phi ge 95) and (phi lt 105) : phi_cor =-.074
           (phi ge 105) and (phi lt 115) : phi_cor =-.060
           (phi ge 115) and (phi lt 125) : phi_cor =-.056
           (phi ge 125) and (phi lt 135) : phi_cor =-.048
           (phi ge 135) and (phi lt 145) : phi_cor =-.034
           (phi ge 145) and (phi lt 155) : phi_cor =-.03
           (phi ge 155) and (phi lt 165) : phi_cor =-.028
           (phi ge 165) and (phi lt 175) : phi_cor =-.026
           (phi ge 175) and (phi lt 185) : phi_cor =-.022
           (phi ge 185) and (phi lt 195) : phi_cor =-.020
           (phi ge 195) and (phi lt 205) : phi_cor =-.016
           (phi ge 205) and (phi lt 215) : phi_cor =-.012
           (phi ge 215) and (phi lt 225) : phi_cor =-.01
           (phi ge 225) and (phi lt 235) : phi_cor =-.006
           (phi ge 235) and (phi lt 245) : phi_cor =-.002
           (phi ge 245) and (phi lt 255) : phi_cor =.008
           (phi ge 255) and (phi lt 265) : phi_cor =.018
           (phi ge 265) and (phi lt 275) : phi_cor =.03
           (phi ge 275) and (phi lt 285) : phi_cor =.044
           (phi ge 285) and (phi lt 295) : phi_cor =.06
           (phi ge 295) and (phi lt 305) : phi_cor =.07
           (phi ge 305) and (phi lt 315) : phi_cor =.08
           (phi ge 315) and (phi lt 325) : phi_cor =.086
           (phi ge 325) and (phi lt 335) : phi_cor =.084
           (phi ge 335) and (phi lt 345) : phi_cor =.074
           (phi ge 345) and (phi lt 355) : phi_cor =.056
           else: print,'phi has an illegal value'
        endcase  
        if sol_pha ge 6 then $
          V_cor = -1.55 + DIST_MAG + 0.021*sol_pha + phi_cor
        if sol_pha lt 6 then $
          V_cor = -1.7233 +  DIST_MAG + $
          0.078*sol_pha - 0.0047*(sol_pha)^2 + phi_cor

        exp1= 26 - (20 + 0.4*V_cor)
        nlam= float((1.509 * 3.694 * 10^(exp1))/6300.304 )
        ;; absolute line flux
        alf = weq * nlam
        err_alf = err_weq * nlam

        intensity=((alf*(206265.^2.)*4.)/((1e6)*((io_dia/2.)^2.)))/1000.
        err_intensity=(err_alf*intensity)/alf

        

        ;; OK, lets collect all the later calculational writes
        ;; together
        dbupdate, entry, 'weq, err_weq, alf, err_alf, p_date, intensity, err_intensity', $
          weq, err_weq, alf, err_alf, today, intensity, err_intensity

        dbclose

     endelse ;; Catching errors
     dbclose

  endfor ;; For each file
  CATCH, /CANCEL

end

