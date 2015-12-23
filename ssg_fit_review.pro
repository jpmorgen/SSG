;+

; $Id: ssg_fit_review.pro,v 1.1 2015/02/17 23:03:19 jpmorgen Exp $

; ssg_fit_review.pro

;-

pro ssg_fit_review

  init = {ssg_sysvar}

  restore, '/data/io/ssg/reduced/1997/971014/sparinfo_2843.sav'
  f_idx = where(sparinfo.pfo.status eq !pfo.active, npar)
  if npar eq 0 then return

  ;; Change to a familiar name and get rid of extra parameters
  parinfo = sparinfo[f_idx]

  ;; Make sure all dgs are consistently assigned
  sso_dg_assign, parinfo

  lc_idx = where(parinfo.sso.ttype eq !sso.center and $
                 parinfo.sso.ptype eq !sso.line, nlines)
  if nlines eq 0 then return

  dgs = parinfo[lc_idx].sso.dg
  rwls = parinfo[lc_idx].sso.rwl

  ;; We want to find dg/rwl pairs, which indicate identical lines.
  ;; Make an array to store the dg/rwl index values of matches
  match_lines = lonarr(nlines)
  u_rwls = uniq(rwls, sort(rwls))
  for irwl=0, N_elements(u_rwls)-1 do begin
     rwl = rwls[u_rwls[irwl]]
     this_rwl_idx = where(rwls eq rwl)
     u_dgs = uniq(dgs[this_rwl_idx], sort(dgs[this_rwl_idx]))
     for idg=0, N_elements(u_dgs)-1 do begin
        dg = dgs[this_rwl_idx[u_dgs[idg]]]
        this_dg_idx = where(dgs[this_rwl_idx] eq dg)
        match_lines[this_rwl_idx[this_dg_idx]] = this_rwl_idx[this_dg_idx[0]]
     endfor ;; each dg in u_dgs
  endfor ;; each rwl in u_rwls
  
  ;; use the array of indices pointing to rwl/dg pairs to pull out the
  ;; unique lines.  Then back in the whole parinfo list, grab all
  ;; their parameters for plotting.
  u_lines = uniq(match_lines, sort(match_lines))
  !p.multi = [0, 0, N_elements(u_lines)*2]
  for il=0, N_elements(u_lines)-1 do begin
     this_line = match_lines[u_lines[il]]
     rwl = rwls[this_line]
     dg = dgs[this_line]
     line_idx = where(parinfo.sso.dg eq dg and $
                      parinfo.sso.rwl eq rwl)
     ew_idx = where(parinfo[line_idx].sso.ttype eq !sso.ew)
     ;; --> this is why I might want to improve the sso transformation
     ;; stuff.
     gw_idx = where(parinfo[line_idx].sso.ttype eq !sso.width and $
                    parinfo[line_idx].sso.pfo.pfo.ftype eq !pfo.voigt + 0.3)
     lw_idx = where(parinfo[line_idx].sso.ttype eq !sso.width and $
                    parinfo[line_idx].sso.pfo.pfo.ftype eq !pfo.voigt + 0.4)
     ;; unnest
     ew_idx = line_idx[ew_idx]
     gw_idx = line_idx[gw_idx]
     lw_idx = line_idx[lw_idx]
     plot, parinfo[ew_idx].ssg.nday, parinfo[ew_idx].value, $
           title=strjoin(sso_dg_path(dg, /name), '-') + ' ' + $
           string(format=!sso.rwl_format, rwl), $
           xtitle='nday', ytitle='Equivalent width (milli A)', psym=!tok.square
     oploterr, parinfo[ew_idx].ssg.nday, parinfo[ew_idx].value, $
               parinfo[ew_idx].error
     
     plot, parinfo[gw_idx].ssg.nday, parinfo[gw_idx].value, $
           title=strjoin(sso_dg_path(dg, /name), '-') + ' ' + $
           string(format=!sso.rwl_format, rwl), $
           xtitle='nday', ytitle='Line width (milli A)', psym=!tok.square, $
           yrange=[min([parinfo[gw_idx].value, parinfo[lw_idx].value]), $
                   max([parinfo[gw_idx].value, parinfo[lw_idx].value])]
     oploterr, parinfo[gw_idx].ssg.nday, parinfo[gw_idx].value, $
               parinfo[gw_idx].error
     oploterr, parinfo[lw_idx].ssg.nday, parinfo[lw_idx].value, $
               parinfo[lw_idx].error, !tok.triangle
     al_legend, ['Gaussian width', 'Lorentzian width'], $
             psym=[!tok.square, !tok.triangle]
     
  endfor
  !p.multi = 0

end
