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
   inner_new_scale=inner_new_scale, $
   xrange=xrange, $
   yrange=yrange, $
   ps=ps, $
   _EXTRA=extra

  init = {tok_sysvar}

  ;; Be polite with the color table
  tvlct, user_r, user_g, user_b, /get
  tek_color

  ;; Mon Jul 17 17:44:52 2017 EDT  jpmorgen@snipe
  ;; Did some work in io.notebk to get these values
  if keyword_set(inner_new_scale) then begin
     low_peak_new_scale = [2.2, 3.2, 2.5, 2.9, 3.0, 3.0, 3.1, 3.0, 3.2, 2.6, 5, 3.7]
     high_peak_new_scale = [5.5, 5.0, 3.3, 3.0, 4.2, 3.3, 3.2, 5.5, 3.5, 4.5, 4.2, 4.5, 5.0, 4.0, 5.3, 5.1, 4.5, 4.2, 4.7]
     inner_new_scales = [low_peak_new_scale, high_peak_new_scale]
     inner_h = histogram(inner_new_scales, binsize=0.5, locations=inner_locations)
     inner_h_err = sqrt(inner_h)
  endif else begin

     ssg_smyth_chi2, sys_III_offset=45, min_data_count=min_data_count, ndays=inner_ndays, long_3s=inner_long_3s, phis=inner_phis, sides=inner_sides, chi2s=inner_chi2s, model_scales=inner_model_scales, /rinner_torus, _EXTRA=extra
  
     inner_ndays = floor(inner_ndays)
     inner_idx = uniq(inner_ndays, sort(inner_ndays))
     inner_h = histogram(inner_model_scales[inner_idx], binsize=0.5, locations=inner_locations)
     inner_h_err = sqrt(inner_h)

     ;; Noticing two peaks in the inner torus, the higher one aligned
     ;; with the scale factor difference on the 1999-10-12 Louarn et
     ;; al. (2014) event.  The dividing line seems to be at 3 (unscaled)
     inner_low_idx = where(inner_model_scales[inner_idx] lt 3.5, complement=inner_high_idx, count, ncomplement=high_count)
     if count gt 0 then begin
        inner_low_idx = inner_idx[inner_low_idx]
        print, 'Ndays with low inner norm value: ', inner_ndays[inner_low_idx]
        print, 'Dates with low inner norm value: ', nday2date(inner_ndays[inner_low_idx])
     endif
     if high_count gt 0 then begin
        inner_high_idx = inner_idx[inner_high_idx]
        print, 'Ndays with high inner norm value: ', inner_ndays[inner_high_idx]
        print, 'Dates with high inner norm value: ', nday2date(inner_ndays[inner_high_idx])
     endif

endelse
  
  ssg_smyth_chi2, sys_III_offset=45, min_data_count=min_data_count, ndays=outer_ndays, long_3s=outer_long_3s, phis=outer_phis, sides=outer_sides, chi2s=outer_chi2s, model_scales=outer_model_scales, /router_torus, _EXTRA=extra
  
  outer_ndays = floor(outer_ndays)
  outer_idx = uniq(outer_ndays, sort(outer_ndays))
  outer_h = histogram(outer_model_scales[outer_idx], binsize=0.5, locations=outer_locations)
  outer_h_err = sqrt(outer_h)

  outer_low_idx = where(outer_model_scales[outer_idx] lt 3.5, complement=outer_high_idx, count, ncomplement=high_count)
  if count gt 0 then begin
     outer_low_idx = outer_idx[outer_low_idx]
     print, 'Ndays with low outer norm value: ', outer_ndays[outer_low_idx]
     print, 'Dates with low outer norm value: ', nday2date(outer_ndays[outer_low_idx])
  endif
  if high_count gt 0 then begin
     outer_high_idx = outer_idx[outer_high_idx]
     print, 'Ndays with high outer norm value: ', outer_ndays[outer_high_idx]
     print, 'Dates with high outer norm value: ', nday2date(outer_ndays[outer_high_idx])
     print, 'Scale factors: ', outer_model_scales[outer_high_idx]
  endif


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

  ;; !! NOTE !! histogram returns locations as the _start_ of each
  ;; bin.  psym=!tok.hist assumes the center
  inner_locations += (inner_locations[1]-inner_locations[0])/2
  outer_locations += (outer_locations[1]-outer_locations[0])/2
  if NOT keyword_set(xrange) then $
     xrange=minmax([inner_locations, outer_locations])
  if NOT keyword_set(yrange) then $
     yrange=minmax([inner_h, outer_h])
  plot, inner_locations, inner_h, psym=!tok.hist, $
        xtitle=xtitle, ytitle=ytitle, $
        xrange=xrange, $
        yrange=yrange, $
        _EXTRA=extra
  oplot, outer_locations, outer_h, psym=!tok.hist, color=!tok.green
  errplot, inner_locations, inner_h-inner_h_err, inner_h+inner_h_err
  errplot, outer_locations, outer_h-outer_h_err, outer_h+outer_h_err, color=!tok.green
  al_legend, ['Inner torus', 'Outer torus'], /top, linestyle=[!tok.solid,!tok.solid], colors=byte([!tok.white, !tok.green]), linsize=0.5, /right

  ;; Tue Sep 19 15:43:24 2017 EDT  jpmorgen@snipe
  ;; print histograms to get number of points in second peak
  print, 'Inner histogram'
  for i=0, N_elements(inner_locations)-1 do $
     print, inner_locations[i], inner_h[i]
  print, 'Outer histogram'
  for i=0, N_elements(outer_locations)-1 do $
     print, outer_locations[i], outer_h[i]

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

