;+
; NAME:
;
; PURPOSE:
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
pro ssg_inner_outer_scale, $
   min_data_count=min_data_count, $
   scale_inner=scale_inner, $
   normalize=normalize, $
   ps=ps, $
   _EXTRA=extra

  init = {tok_sysvar}

  ;; Be polite with the color table
  tvlct, user_r, user_g, user_b, /get
  tek_color

  ssg_smyth_chi2, sys_III_offset=45, min_data_count=min_data_count, ndays=inner_ndays, long_3s=inner_long_3s, phis=inner_phis, sides=inner_sides, chi2s=inner_chi2s, model_scales=inner_model_scales, /inner_torus
  
  ssg_smyth_chi2, sys_III_offset=45, min_data_count=min_data_count, ndays=outer_ndays, long_3s=outer_long_3s, phis=outer_phis, sides=outer_sides, chi2s=outer_chi2s, model_scales=outer_model_scales, /outer_torus
  
  inner_ndays = floor(inner_ndays)
  inner_idx = uniq(inner_ndays, sort(inner_ndays))
  inner_h = histogram(inner_model_scales[inner_idx], binsize=0.5, locations=inner_locations)
  inner_h_err = sqrt(inner_h)
  
  outer_ndays = floor(outer_ndays)
  outer_idx = uniq(outer_ndays, sort(outer_ndays))
  outer_h = histogram(outer_model_scales[outer_idx], binsize=0.5, locations=outer_locations)
  outer_h_err = sqrt(outer_h)


  ;; This doesn't allow for min_data_count
  ;;model_top = !ssg.top + path_sep() + 'analysis' + path_sep() + 'max' +  path_sep()
  ;;restore, model_top + 'max_1_line_template.sav'
  ;;model = read_ascii(model_top + 'brit_set3_N216', template=max_1_line_template)
  ;;
  ;;inner_idx = where(30 lt model.phi and model.phi lt 180)
  ;;inner_model_scales = model.model_scale[inner_idx]
  ;;
  ;;outer_idx = [where(model.phi lt 30), where(model.phi gt 180)]
  ;;outer_model_scales = model.model_scale[outer_idx]
  ;;
  ;;inner_ndays = floor(model.max_inday[inner_idx])
  ;;inner_idx = uniq(inner_ndays, sort(inner_ndays))
  ;;inner_h = histogram(inner_model_scales[inner_idx], binsize=0.5, locations=inner_locations)
  ;;inner_h_err = sqrt(inner_h)
  ;;
  ;;outer_ndays = floor(model.max_inday[outer_idx])
  ;;outer_idx = uniq(outer_ndays, sort(outer_ndays))
  ;;outer_h = histogram(outer_model_scales[outer_idx], binsize=0.5, locations=outer_locations)
  ;;outer_h_err = sqrt(outer_h)

  xtitle = 'Scale factor'
  if keyword_set(scale_inner) then begin
     inner_locations *= scale_inner
     xtitle = string(format='(a, "; inner torus scaled by ", F3.1)', xtitle, scale_inner)
  endif

  ytitle = 'Histogram'
  if keyword_set(normalize) then begin
     ytitle = 'Normalized ' + ytitle
     inner_h_err = inner_h_err/total(inner_h)
     inner_h = inner_h/total(inner_h)
     outer_h_err = outer_h_err/total(outer_h)
     outer_h = outer_h/total(outer_h)
  endif

  ;; Initialize postscript output
  if keyword_set(ps) then begin
     ;; Be polite, but get the line thicknesses we need for PS output
     ;; (leave for terminal output since that gets us started on PS output)
     oPthick     = !P.thick
     oPcharsize  = !P.charsize
     oPcharthick = !P.charthick
     oXthick     = !X.thick
     oYthick     = !Y.thick

     !P.thick = 3
     !P.charsize = 1.5
     !P.charthick = 2
     !X.thick = 2
     !Y.thick = 2
     
     ;; Correct white/black problem in al_legend
     tmp = !tok.white
     !tok.white = !tok.black 
     !tok.black = tmp

     if size(ps, /TNAME) ne 'STRING' then $
        ps = 'ssg_inner_outer_scale_out.ps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /color, /encap
  endif ;; initialize PS

  plot, inner_locations, inner_h, psym=!tok.hist, xtitle=xtitle, ytitle=ytitle, yrange=minmax([inner_h, outer_h]), $
        _EXTRA=extra
  oplot, outer_locations, outer_h, psym=!tok.hist, color=!tok.green
  errplot, inner_locations, inner_h-inner_h_err, inner_h+inner_h_err
  errplot, outer_locations, outer_h-outer_h_err, outer_h+outer_h_err, color=!tok.green
  al_legend, ['Inner torus', 'Outer torus'], /top, linestyle=[!tok.solid,!tok.solid], colors=byte([!tok.white, !tok.green]), linsize=0.5, /right

  if keyword_set(ps) then begin
     device,/close
     set_plot, 'x'
     ;; Put white/black back
     tmp = !tok.white
     !tok.white = !tok.black 
     !tok.black = tmp

     !P.thick     = oPthick    
     !P.charsize  = oPcharsize 
     !P.charthick = oPcharthick
     !X.thick     = oXthick    
     !Y.thick     = oYthick    

  endif  

  ;; Return color table to its original value
  tvlct, user_r, user_g, user_b

end

