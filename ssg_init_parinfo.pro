; +
; $Id: ssg_init_parinfo.pro,v 1.1 2002/12/16 13:36:14 jpmorgen Exp $

; ssg_init_parinfo.pro  Make num blank parinfo records appropriate for the Io
; SSG fitting system.  See ssg_fit1spec.pro documentation

; -

function ssg_init_parinfo, num, voigt=voigtin

  if N_params() eq 0 then num = 1
  if num lt 1 then return, 0

  if NOT keyword_set(voigtin) then begin
     
     parinfo = { fixed:0, $
                 limited:[0,0], $
                 limits:[0.D,0.D], $
                 parname:'', $
                 tied:'', $
                 vfID:0, $
                 ssgID:0, $
                 ssggroupID:0, $
                 ssgrwl:0D, $
                 ssgowl:0D, $
                 ssglink:'', $
                 ssgdop:0 }

     return, replicate(parinfo, num)

  endif  ;; Just num blank records

  ;; Fancier structure (just Voigt for now)
  if num gt 1 then message, 'ERROR: you can only intialize one Voigt at a time'
  ;; Don't modify voigtin
  voigt = voigtin

  ;; voigt = [rwl, dop width, lor width, area]
  ;; Generic defaults for widths and areas
  while N_elements(voigt) lt 4 do voigt = [voigt, 0]
  if voigt[1] eq 0 then voigt[1] = 0.1 ; Angtroms
  if voigt[2] eq 0 then voigt[2] = 0 ; Turn off Lorentzian
  if voigt[3] eq 0 then voigt[3] = 10.

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

  ;; Initializing 4 parinfo records for a Voigt.  
  parinfo = ssg_init_parinfo(4)

  ;; Important step: save off the rest wavelegnth to parinfo.ssgrwl
  ;; for all parameters, but put the observed wavelength (which may
  ;; eventually be Doppler shifted) into the .ssgowl field of ONLY the
  ;; line center parameter.  That way Doppler shifts can be handled
  ;; without explicit knowledge of the function
  parinfo[*].ssgrwl = voigt[0]
  parinfo[0].ssgowl = voigt[0]

  lname = string(format='(f9.4)', voigt[0])
  ;; Now set the wavelength to 0 since we will be fitting it as
  ;; delta wavelength 
  voigt[0] = 0.D

  vpnames = ['Dlambda', 'DopFWHM', 'LorFWHM', 'Area']
  for ip = 0,3 do begin
     ;; Set limits to initial values so mpfit won't complain when it
     ;; starts (even if limited turned off).  This is sort of a pain,
     ;; but doesn't constrain things later on in the fit.  Just
     ;; remember to reset parinfo.limits whenever you turn around and
     ;; do another fit.
     parinfo[ip].limits = [voigt[ip], voigt[ip]]
     parinfo[ip].parname = string(lname, ' ', vpnames[ip])
     parinfo[ip].vfID = ip + 2
  endfor
  parinfo.ssgID[*] = ssgid_voigt

  return, parinfo

end
