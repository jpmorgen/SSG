;+
; NAME: ssg_ana_close_lines
;
; PURPOSE: Plot and analyze line parameters that are close to each
; other to help find stable parameter values.  Relies on .sav file
; from ssg_fit2ana 
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id:$
;
; $Log:$
;-
pro ssg_ana_close_lines, fname
  init = {ssg_sysvar}
  init = {tok_sysvar}
  ;; Restore our sssg_ana_parinfo.  Let IDL raise the error if file
  ;; not found
  restore, fname, /relaxed_structure_assignment
  ;; Code from ssg_fit2ana.pro
  if N_elements(sssg_ana_parinfo) eq 0 then $
     message, 'ERROR: no saved parinfo found for this entire nday'

  ;; Get the indices into our line center parameters
  lc_idx = where(sssg_ana_parinfo.sso.ttype eq !sso.center and $
                 sssg_ana_parinfo.sso.ptype eq !sso.line, count)
  if count eq 0 then $
     message, 'ERROR: no lines found'

  ;; Cycle through our Doppler groups first, then RWL, since at least
  ;; for Io and airglow, [OI] is the same RWL.

  dgs = sssg_ana_parinfo[lc_idx].sso.dg
  dgs = dgs[uniq(dgs, sort(dgs))]
  for idg=0, N_elements(dgs)-1 do begin
     tdg_idx = where(sssg_ana_parinfo[lc_idx].sso.dg eq dgs(idg), N_lines)
     if N_lines eq 0 then $
        message, 'ERROR: something is really wrong!  I should find the dg I just found!'
     ;; unwrap
     tdg_idx = lc_idx[tdg_idx]
     
     ;; Get our list of unique, sorted rest wavelengths
     RWLs = sssg_ana_parinfo[tdg_idx].sso.RWL
     RWLs = RWLs[uniq(RWLs, sort(RWLs))]

     ;; Go through each line in this Doppler group
     for il=0, N_elements(RWLs)-1 do begin
        ;; Grab only the matching line centers
        tl_lc_idx = where(sssg_ana_parinfo[lc_idx].sso.dg eq dgs[idg] and $
                          sssg_ana_parinfo[lc_idx].sso.RWL eq RWLs[il], count)
        ;; Do our sanity checks
        if count eq 0 then $
           message, 'ERROR: not finding things I just found!'
        ;; Check to see if there are enough points to plot
        if count lt 2 then $
           CONTINUE
        ;; unwrap
        tl_lc_idx = lc_idx[tl_lc_idx]
        bad_idx = where(floor(sssg_ana_parinfo[tl_lc_idx].sso.pfo.pfo.ftype) ne !pfo.voigt, count)
        if count gt 0 then $
           message, 'ERROR: I expect everything to com up Voigts right now'
        ;; Plot all parameters together
        erase
        !p.multi = [0, 1, 4]
        ;; Start out with just the closest line
        !P.charsize = 2
        for ipar=0,!pfo.fnpars[!pfo.voigt]-1 do begin
           xtitle = string(format='("Delta observed wavelength (A) from ", f9.4, " A ", a )', RWLs[il], $
                          sssg_ana_parinfo[tl_lc_idx[0]+1].parname)
           plot, sssg_ana_parinfo[tl_lc_idx+ipar].sso_ana.DOWL[0], $
                 sssg_ana_parinfo[tl_lc_idx+ipar].sso_ana.value[0], $
                 xtitle=xtitle, $
                 ytitle=sssg_ana_parinfo[tl_lc_idx[0]+ipar].sso.pfo.parname, $
                    psym=!tok.plus, $
                    xrange=xrange, xstyle=!tok.exact+!tok.extend
           oploterr, sssg_ana_parinfo[tl_lc_idx+ipar].sso_ana.DOWL[0], $
                     sssg_ana_parinfo[tl_lc_idx+ipar].sso_ana.value[0], $
                     !tok.dot
        endfor;; Each parameter
        wait, 1
        
     endfor ;; each line
  endfor ;; each Doppler group

end

