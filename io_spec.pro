; $Id: io_spec.pro,v 1.1 2002/12/16 18:25:30 jpmorgen Exp $

;; AUTODERIVATIVE must be set to 1 since I don't calculate analytic
;; derivatives (a 3rd parameter)
function io_spec, pix_X, params, parinfo=parinfo, $
                  ref_pixel=ref_pixel, spec=spec, err_spec=err_spec

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

  c = 299792.458 ;; km/s
  
  ;; Since pix_X might have holes in it, this calculates an
  ;; anatomically correct wavelength at each point, no matter how much
  ;; space there is relative to its neighbors
  disp_idx = where(parinfo.ssgID eq ssgid_disp, count)
  if count gt 0 then $
    X = make_disp_axis(params[disp_idx], pix_X, ref_pixel) $
  else $
    X = pix_X

  ;; Doppler shifts
  dop_idx = where(parinfo.ssgID eq ssgid_dop, n_dop)
  ;; For each doppler group find the line center parameters in that group
  for id=0,n_dop-1 do begin
     dv = params[dop_idx[id]]
     dop_group = parinfo[dop_idx[id]].ssgdop
     ;; Pick up all the lines but not the Doppler parameter itself!
     lc_idx = where(parinfo.ssgdop eq dop_group and $
                    parinfo.ssgID ne ssgid_dop, n_lines)
     ;; For each line in that group
     for il = 0,n_lines-1 do begin
        parinfo[lc_idx[il]].ssgowl = $
          parinfo[lc_idx[il]].ssgrwl * ( 1. + dv / c )
     endfor
     ;; I am not going to tweak lc (line center) parameters since they
     ;; are one of the things that is being fit
  endfor
  

  ;; voigtspec is not as delicate when it comes to its parameter list
  ;; as Voigfn, so I can just pass all the parameters right on through

  Y = voigtspec(X, params, parinfo=parinfo, Y=Y)

  ;; Give user a chance to see plots and abort fit.  Craig might have
  ;; a better way of doing this, but this is what I have for now.
  answer = strupcase(get_kbrd(0))
  if answer eq 'D' then begin
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

     wset,7
     !p.multi=[0,0,2]
     plot, X, spec, psym=dot, title='Intermediate Result', $
           xtitle=xtitle, ytitle=ytitle, $
           xrange=[min(X), max(X)], yrange=[min(spec), max(spec)], $
           xstyle=1, ystyle=2
     oplot, X, Y, linestyle=dotted
     oploterr, X, spec, err_spec, dot
     residual = spec - Y
     plot, X, residual, $
           title='Fit residual', xtitle=xtitle, ytitle=ytitle, $
           xstyle=1, ystyle=2, psym=10
     !p.multi=0
  endif
  if answer eq 'S' then message, 'STOPPING FIT'

  return, Y

end

