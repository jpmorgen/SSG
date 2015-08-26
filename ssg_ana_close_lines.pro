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
pro ssg_ana_close_lines, fname, noninteractive=noninteractive
  init = {ssg_sysvar}
  init = {tok_sysvar}
  opcolor = !p.color
  color
  ;; Be polite with the color table
  tvlct, user_r, user_g, user_b, /get
  tek_color
  ;; Make array of Y-axes titles.  These are not stored in the
  ;; .parnames anywhere (see pfo_sso_funct.pro)
  ytitles = ['abs(Dop+dw)', 'ew', 'Gw', 'Lw'] + ' (m'+string("305B)+')' ;"
  ytitles = ['Red chi!U2!D', ytitles]
  ;; Restore our sssg_ana_parinfo.  Let IDL raise the error if file
  ;; not found
  restore, fname, /relaxed_structure_assignment
  ;; Sanity checks
  N_sssg_ana_parinfo = N_elements(sssg_ana_parinfo) 
  if N_sssg_ana_parinfo eq 0 then $
     message, 'ERROR: no saved parinfo found'

  ;; Get the indices into our line center parameters
  lc_idx = where(sssg_ana_parinfo.sso.ttype eq !sso.center and $
                 sssg_ana_parinfo.sso.ptype eq !sso.line, count)
  if count eq 0 then $
     message, 'ERROR: no lines found'

  ;; If we made it here, we have some business to do.
  
  ;; Get our Doppler groups (dgs) set up.  dgs are ephemeral, so they
  ;; need to be assigned and stored in the sso_sysvar pointer system
  ;; to work properly
  sso_dg_assign, sssg_ana_parinfo
  ;; Get a unique list of dgs in the ssg_ana_parinfo, since these are
  ;; the ones we want to key in on
  dgs = sssg_ana_parinfo[lc_idx].sso.dg
  dgs = dgs[uniq(dgs, sort(dgs))]
  N_dgs = N_elements(dgs)
  
  ;; Get our original line list, lparinfo for comparing to the final
  ;; fit and modifying for the next fit
  ssg_lparinfo, [6280, 6320]
  lparinfo = *!ssg.lparinfo
  ;; The lparinfo uses a general !sso.obj path element.  Get those
  ;; Doppler groups assigned and fix them when I need them in the
  ;; plotting/analysis section, below
  sso_dg_assign, lparinfo

  ;; Cycle through our sssg_ana_parinfo Doppler groups first, then
  ;; RWL, since at least for Io and airglow, [OI] is the same RWL.

  for idg=0, N_dgs-1 do begin
     tdg_idx = where(sssg_ana_parinfo[lc_idx].sso.dg eq dgs(idg), N_lines)
     if N_lines eq 0 then $
        message, 'ERROR: something is really wrong!  I should find the dg I just found!'
     ;; unwrap
     tdg_idx = lc_idx[tdg_idx]

     ;; Figure out what our object is in this path.  Put Jupiter in
     ;; just in case we ever fit scattered light off of Jupiter
     path = sso_dg_path(sssg_ana_parinfo[tdg_idx[0]].sso.dg)
     path_obj_idx = where(path ne !eph.sun and $
                          path ne !eph.earth and $
                          path ne !eph.jupiter and $
                          path ne !sso.aterm, $
                          N_obj)
     if N_obj gt 1 then $
        message, 'ERROR: I don''t expect more than one object (e.g. Io, Europa, etc.) on the path'
     ;; Translate the generic object dg in lparinfo to the specific
     ;; object dg of the sssg_ana_lparinfo, if we have one
     if N_obj eq 1 then begin
        obj = path[path_obj_idx]
        ;; Start from our sssg_ana_parinfo path
        lparinfo_path = path
        ;; Replace the object element with our generic lparinfo object code
        lparinfo_path[path_obj_idx] = !eph.obj
        ;; Find that dg
        lparinfo_dg = sso_path_dg(lparinfo_path)
        ;; Replace the dgs temporarily with the sssg_ana_parinfo dg
        lparinfo_dg_idx = where(lparinfo.sso.dg eq lparinfo_dg, count)
        if count eq 0 then $
           message, 'ERROR: no Doppler group paramters found.  This should not happen!'
        lparinfo[lparinfo_dg_idx].sso.dg = dgs(idg)        
     endif ;; translating lparinfo dg

     ;; Get a path listing
     path_names = strjoin(sso_dg_path(dgs(idg),  /names), '-')

     ;; Get our list of unique, sorted rest wavelengths
     RWLs = sssg_ana_parinfo[tdg_idx].sso.RWL
     RWLs = RWLs[uniq(RWLs, sort(RWLs))]

     ;; Go through each line in this Doppler group.
     for il=0, N_elements(RWLs)-1 do begin
        ;; Handle the case when we are going back to a previous dg.
        ;; We want to start at the top of our previous dg's line list
        if keyword_set(dg_going_backward) then begin
           dg_going_backward = 0
           il = N_elements(RWLs)-1
        endif
        print, 'il =', il, '  idg = ', idg
        ;; Grab only the matching line centers
        tl_lc_idx = where(sssg_ana_parinfo[lc_idx].sso.dg eq dgs[idg] and $
                          sssg_ana_parinfo[lc_idx].sso.RWL eq RWLs[il], count)
        ;; Do our sanity checks
        if count eq 0 then $
           message, 'ERROR: not finding things I just found!'
        ;; Check to see if there are enough points to plot.
        if count lt 2 then begin
           ;; Check to see if we are going backward
           if keyword_set(going_backward) then begin
              ;; If so, run the for loop backward
              il -= 2
              ;; until we get too low, in which case we trigger the
              ;; end of the il loop and go to the previous idg
              if il lt -1 then begin
                 il = N_elements(RWLs)-1
                 idg -= 2
                 dg_going_backward = 1
              endif
              ;; Unless we go below the 1st Doppler group
              if idg lt -1 then begin
                 idg = 0
                 il = 0
                 dg_going_backward = 0
                 going_backward = 0
              endif
           endif ;; going backward
           CONTINUE
        endif ;; checking enough points to plot

        ;; If we made it here, we have enough points to plot.

        ;; unwrap
        tl_lc_idx = lc_idx[tl_lc_idx]
        bad_idx = where(floor(sssg_ana_parinfo[tl_lc_idx].sso.pfo.pfo.ftype) ne !pfo.voigt, count)
        if count gt 0 then $
           message, 'ERROR: I expect everything to com up Voigts right now'
        ;; Grab the same lines from lparinfo
        lp_lc_idx = where(lparinfo.sso.dg eq dgs[idg] and $
                          lparinfo.sso.RWL eq RWLs[il] and $
                          lparinfo.sso.ttype eq !sso.center and $
                          lparinfo.sso.ptype eq !sso.line, count)
        if count eq 0 then $
           message, 'ERROR: line not found in lparinfo'

        ;; Plot all parameters together
        !p.multi = [0, 1, 5]
        !P.charsize = 2
        ;; They all share one X-axis.  Start out with our closest line
        xaxis = sssg_ana_parinfo[tl_lc_idx].sso_ana.DOWL[0]
        line_id = string(format='(a, " ", f9.4, " ", a, a)', $
                         path_names, $
                         RWLs[il], $
                         string("305B), $ ;"
                         sssg_ana_parinfo[tl_lc_idx[0]+1].parname)
        xtitle = string(format='("Delta observed wavelength (", a, ") from ", a)', $
                        string("305B), $ ;"
                        line_id)
                        
        for iplot=0,1+!pfo.fnpars[!pfo.voigt]-1 do begin
           ipar = 0
           ylog = 0
           case iplot of
              0 : begin
                 ;; Reduced chi2.  Plot it on a log scale
                 par_values = sssg_ana_parinfo[tl_lc_idx+ipar].sso_ana.redchisq
                 err_values = 0
                 ylog = 1
              end
              1 : begin
                 ;; Combine Delta Doppler + dw
                 par_values = $
                    abs(sssg_ana_parinfo[tl_lc_idx+ipar].sso_ana.delta_dop) + $
                    abs(sssg_ana_parinfo[tl_lc_idx+ipar].value)
                 err_values = $
                    sqrt(sssg_ana_parinfo[tl_lc_idx+ipar].sso_ana.err_delta_dop^2 + $
                         sssg_ana_parinfo[tl_lc_idx+ipar].error^2)
                 ylog = 1
              end
              else : begin
                 ;; Generic Voigt parameter 
                 ipar = iplot - 1
                 ;; Plot the value and error of our host line as a function
                 ;; DOWL to the 0th closest line
                 par_values = sssg_ana_parinfo[tl_lc_idx+ipar].value
                 err_values = sssg_ana_parinfo[tl_lc_idx+ipar].error
              end
           endcase
           ;; Cycle through the dgs of the close lines to plot each
           ;; one in a different color.  Start by making the plot axes
           plot, xaxis, par_values, /nodata, ylog=ylog, $
                 xmargin=[12,3], $
                 xtitle=xtitle, $
                 ytitle=ytitles[iplot], $
                 psym=!tok.plus, $
                 xrange=xrange, xstyle=!tok.exact+!tok.extend, $
                 ystyle=!tok.extend
           for ipdg=0, N_elements(dgs)-1 do begin
              clp_idx = where(sssg_ana_parinfo[tl_lc_idx].sso_ana.dg[0] $
                              eq dgs[ipdg], count)
              ;;;; Avoid trouble with not enough points
              if count lt 2 then $
                 CONTINUE
              ;; Don't unwrap.  clp_idx is the index into
              ;; tl_lc_idx+ipar, which is what xaxis and par_values
              ;; were created from
              oplot, xaxis[clp_idx], $
                     par_values[clp_idx], $
                     psym=!tok.plus, $
                     color=dgs[ipdg]
              errplot, xaxis[clp_idx], $
                       par_values[clp_idx] + err_values[clp_idx]/2, $
                       par_values[clp_idx] - err_values[clp_idx]/2, $
                       color=dgs[ipdg], width=0.001
              ;;oploterr, xaxis[clp_idx], $
              ;;          par_values[clp_idx], $
              ;;          err_values[clp_idx], !tok.dot
           endfor ;; plotting each close line Doppler group by color

           ;; Plot lparinfo value
           oplot, !x.crange, $
                  replicate(lparinfo[lp_lc_idx+ipar].value, 2), $
                  linestyle=!tok.solid
           med = median(par_values)
           stdev = stdev(par_values)
           oplot, !x.crange, replicate(med, 2), $
                  linestyle=!tok.dashed
           oplot, !x.crange, replicate(med + stdev, 2), $
                  linestyle=!tok.dotted
           oplot, !x.crange, replicate(med - stdev, 2), $
                  linestyle=!tok.dotted
        endfor;; Each parameter

        ;; Make a nice menu for paging through the lines
        answer = ''
        if NOT keyword_set(noninteractive) then begin
           line_id = string(format='(a, " ", f9.4, " A ", a)', $
                            path_names, $
                            RWLs[il], $
                            sssg_ana_parinfo[tl_lc_idx[0]+1].parname)

           print, line_id

           message, /CONTINUE, 'Menu:'
           print, 'Next'
           print, 'Previous'
           print, 'free-Run'
           print, 'Quit in mid-run'
           answer = ''
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, '[N], P, R, Q?'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'N'
              for ki = 0,1000 do flush_input = get_kbrd(0)
              answer = strupcase(answer)
           endrep until $
              answer eq 'N' or $
              answer eq 'P' or $
              answer eq 'R' or $
              answer eq 'Q'
           case answer of
              'N' : going_backward = 0 ;; Just let for loop continue
              'P' : begin
                 ;; Make the for loop go backward and make a flag that
                 ;; tells us go backward in our search for enough
                 ;; lines to print
                 il -= 2
                 going_backward = 1
                 ;; Check to see if we are too low, in which case we
                 ;; trigger the end of the il loop and go to the
                 ;; previous idg
                 if il lt -1 then begin
                    il = N_elements(RWLs)-1
                    idg -= 2
                    dg_going_backward = 1
                 endif
                 ;; Unless we go below the 1st Doppler group.
                 if idg lt -1 then begin
                    idg = 0 ;; still in the idg loop
                    il = -1 ;; cycle back to 0 (I think)
                    dg_going_backward = 0
                    going_backward = 0
                 endif
              end
              'R' : begin
                 noninteractive = 1
                 going_backward = 0
              end
              'Q' : begin
                 ;; Kick out the loops to finish
                 il = N_elements(RWLs)-1
                 idg = N_elements(dgs)-1
              end              
           endcase
        endif ;; Interactive
     endfor ;; each line
     ;; Reset lparinfo dg to generic state
     if N_obj eq 1 then begin
        lparinfo[lparinfo_dg_idx].sso.dg = lparinfo_dg
     endif ;; translate lparinfo dg back to generic value
  endfor ;; each Doppler group
  ;; Return color table to its original value
  tvlct, user_r, user_g, user_b
  opcolor = !p.color
  !p.multi = 0
end
