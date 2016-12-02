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
pro ssg_inner_outer_nn_dowl, $
   ps=ps, $
   histogram=histogram, $
   err_histogram=err_histogram, $
   normalize=normalize

  init = {tok_sysvar}

  ;; Be polite with the color table
  tvlct, user_r, user_g, user_b, /get
  tek_color

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
        ps = 'ssg_inner_outer_nn_DOWL_out.ps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /color, /encap

  endif

  adbname = 'io_oi_analyze'
  dbopen, adbname, 0
  all_io = dbfind("obj_code=1", dbfind("err_intensity<10"))
  inner_torus = dbfind("io_phi<180", dbfind("io_phi>30", all_io))
  outer_torus = [dbfind("io_phi>180", all_io), dbfind("io_phi<30", all_io)]
  dbext, inner_torus, 'nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc, nn_DOWL, nn_EW, nn_LW', nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc, nn_DOWL, nn_EW, nn_LW
  
  plot, intensity, nn_DOWL, psym=!tok.triangle, xtitle='Intensity (kR)',  ytitle='nn_DOWL', yrange=[-400,400]
;;plot, long_3, nn_DOWL, psym=!tok.dot, color=!tok.green

  ;; Make a histogram of intensity
  h_inner = histogram(intensity)
  h_err_inner = histogram(err_intensity, binsize=0.2, locations=err_inner_positions)

  dbext, outer_torus, 'nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc, nn_DOWL, nn_EW, nn_LW', nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc, nn_DOWL, nn_EW, nn_LW

  dbclose

  ;; Make a histogram of intensity
  h_outer = histogram(intensity)
  h_err_outer = histogram(err_intensity, binsize=0.2, locations=err_outer_positions)

  oplot, intensity, nn_DOWL, psym=!tok.square, color=!tok.green
;;oplot, long_3, nn_DOWL, psym=!tok.dot

  al_legend, ['Inner torus', 'Outer torus'], /top, psym=[!tok.triangle,!tok.square], colors=byte([!tok.white, !tok.green]), /right
  
  if keyword_set(ps) then begin
     device,/close
     set_plot, 'x' 
     !P.thick     = oPthick    
     !P.charsize  = oPcharsize 
     !P.charthick = oPcharthick
     !X.thick     = oXthick    
     !Y.thick     = oYthick    
     ;; Put white/black back
     tmp = !tok.white
     !tok.white = !tok.black 
     !tok.black = tmp
  endif  

  if keyword_set(histogram) then begin
     h_inner_err = sqrt(h_inner)
     h_outer_err = sqrt(h_outer)
     if keyword_set(normalize) then begin
        h_inner_err = h_inner_err/total(h_inner)
        h_outer_err = h_outer_err/total(h_outer)
        h_inner = h_inner/total(h_inner)
        h_outer = h_outer/total(h_outer)
     endif
     plot, h_inner, psym=!tok.hist, xtitle='Intensity (kR)', ytitle='Histogram', yrange=minmax(h_outer)
     oplot, h_outer, psym=!tok.hist, color=!tok.green
     errplot, h_inner-h_inner_err, h_inner+h_inner_err
     errplot, h_outer-h_outer_err, h_outer+h_outer_err, color=!tok.green
     al_legend, ['Inner torus', 'Outer torus'], /top, psym=[!tok.triangle,!tok.square], colors=byte([!tok.white, !tok.green]), /right
  endif ;; intensity histogram (similar)
  
  if keyword_set(err_histogram) then begin
     ;; Horrible name substitution
     h_inner = h_err_inner
     h_outer = h_err_outer
     h_inner_err = sqrt(h_inner)
     h_outer_err = sqrt(h_outer)
     if keyword_set(normalize) then begin
        h_inner_err = h_inner_err/total(h_inner)
        h_outer_err = h_outer_err/total(h_outer)
        h_inner = h_inner/total(h_inner)
        h_outer = h_outer/total(h_outer)
     endif
     plot, err_inner_positions, h_inner, psym=!tok.hist, xtitle='Err Intensity (kR)', ytitle='Histogram', yrange=minmax(h_outer)
     oplot, err_outer_positions,  h_outer, psym=!tok.hist, color=!tok.green
     errplot, err_inner_positions, h_inner-h_inner_err, h_inner+h_inner_err
     errplot, err_outer_positions, h_outer-h_outer_err, h_outer+h_outer_err, color=!tok.green
     al_legend, ['Inner torus', 'Outer torus'], /top, psym=[!tok.triangle,!tok.square], colors=byte([!tok.white, !tok.green]), /right
     
  endif ;; intensity error histogram.  Inner definitely has a tail to high error bars

  ;; Return color table to its original value
  tvlct, user_r, user_g, user_b

end
