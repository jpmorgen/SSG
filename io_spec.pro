; $Id: io_spec.pro,v 1.2 2015/03/04 16:01:22 jpmorgen Exp $

;; Calculate a model spectrum over the given pixel axis with params
;; and parinfo.  This is where the conversion between rest wavelength
;; and observed wavelength is made using the Doppler shifts, and where
;; the equivalent width is calculated.

;; AUTODERIVATIVE must be set to 1 since I don't calculate analytic
;; derivatives (a 3rd parameter)
function io_spec, pix_X, params, parinfo=parinfo, $
                  ref_pixel=ref_pixel, spec=spec, err_spec=err_spec

  ;; Make tokens for everything
  vfid_cont = 1
  vfid_center = 2
  vfid_area = 3
  vfid_ew = 3
  vfid_dop = 4
  vfid_lor = 5
  vfid_first = vfid_center
  vfid_last = vfid_lor

  ssgid_disp = 1
  ssgid_dop = 2
  ssgid_cont = 3
  ssgid_voigt = 4

  c = 299792.458 ;; km/s
  
  ;; Check to see if this is really an Io spectrum
  cont_idx = where(parinfo.ssgID eq ssgid_cont, n_cont)
  if n_cont eq 0 then $
    message, 'ERROR: all Io spectra should have a continuum'

  ;; Build X and Y axes.  Since pix_X might have holes in it, this
  ;; calculates an anatomically correct wavelength at each point, no
  ;; matter how much space there is relative to its neighbors
  disp_idx = where(parinfo.ssgID eq ssgid_disp, count)
  if count gt 0 then $
    X = make_disp_axis(params[disp_idx], pix_X, ref_pixel, /scaled) $
  else $
    X = pix_X

  ;; Put the continuum in the Y axis.  Note that continuum is
  ;; referenced to ref_pixel and the pixel axis in general, not to the
  ;; wavelength axis.  This makes things better behaved numerically,
  ;; since the wavelength axis can shift around.
  Y = dblarr(N_elements(X))
  for n=0,n_cont-1 do begin
     Y = Y + params[cont_idx[n]]*(pix_X-ref_pixel)^n
  endfor

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
  
  ;; We are going to want to make a new parameter list for voigtfn
  ;; with just the Voigt parameters in it.  Since voigfn doesn't use
  ;; parinfo and doesn't accept continuum arguments, we can make this
  ;; very simple
  vparams=params
  voigt_idx = where(vfid_first le parinfo.vfid and $
                    parinfo.vfid le vfid_last, nv)

  ;; Return continuum if no Voigts
  if nv eq 0 then return, Y

  ;; All of the Io Voigt parameters we are fitting are in mA, but the
  ;; wavelength axis is in A, so convert
  vparams[voigt_idx] = vparams[voigt_idx]/1000d

  ;; Voigtfn takes the absolute wavelength, so we need to build that
  ;; from parinfo.ssgowl and the delta line center parameter
  lc_idx = where(parinfo.vfID eq vfid_center, nv)  

  for iv=0, nv-1 do begin
     idx = lc_idx[iv]
     vparams[idx] = vparams[idx] + parinfo[idx].ssgowl
  endfor

  ;; Now interpolate back to the pixel axis so we can read the
  ;; continuum level at the line center for the equivalent width
  ;; calculation
  pix_centers = interpol(pix_X, X, vparams[lc_idx], /quadratic)
  cont_values = dblarr(nv)
  for iv=0, nv-1 do begin
     cont_values[iv] = poly(pix_centers[iv]-ref_pixel, params[cont_idx])
  endfor

  ;; Voigtfn takes absolute area, so calculate that from the
  ;; equivalent width, which is in mA
  a_idx = where(parinfo.vfID eq vfid_area, nv)  
  for iv=0, nv-1 do begin
     idx = a_idx[iv]
     vparams[idx] = vparams[idx] * cont_values[iv]
  endfor

  Y = voigtfn(vparams[voigt_idx], X, Y)
  
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
           xstyle=1, ystyle=2, psym=dot
     oploterr, X, residual, err_spec, dot
     !p.multi=0
  endif
  if answer eq 'S' then message, 'STOPPING FIT'

  return, Y

end

