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
   sysIV=sysIV, $
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

     ssg_smyth_chi2, sys_III_offset=45, min_data_count=min_data_count, ndays=inner_ndays, long_3s=inner_long_3s, phis=inner_phis, sides=inner_sides, chi2s=inner_chi2s, model_scales=inner_model_scales, /rinner_torus, plasma_rs=inner_plasma_rs, _EXTRA=extra
  
     ;; Integer inner ndays so we can still use the original ndays, below
     iinner_ndays = floor(inner_ndays)
     inner_idx = uniq(iinner_ndays, sort(iinner_ndays))
     inner_h = histogram(inner_model_scales[inner_idx], binsize=0.5, locations=inner_locations)
     inner_h_err = sqrt(inner_h)

     ;; Noticing two peaks in the inner torus, the higher one aligned
     ;; with the scale factor difference on the 1999-10-12 Louarn et
     ;; al. (2014) event.  The dividing line seems to be at 3 (unscaled)
     inner_low_idx = where(inner_model_scales[inner_idx] lt 3.5, complement=inner_high_idx, count, ncomplement=high_count)
     if count gt 0 then begin
        inner_low_idx = inner_idx[inner_low_idx]
        print, 'Ndays with low inner norm value: ', iinner_ndays[inner_low_idx]
        print, 'Dates with low inner norm value: ', nday2date(iinner_ndays[inner_low_idx])
     endif
     if high_count gt 0 then begin
        inner_high_idx = inner_idx[inner_high_idx]
        print, 'Ndays with high inner norm value: ', iinner_ndays[inner_high_idx]
        print, 'Dates with high inner norm value: ', nday2date(iinner_ndays[inner_high_idx])
     endif

endelse
  
  ssg_smyth_chi2, sys_III_offset=45, min_data_count=min_data_count, ndays=outer_ndays, long_3s=outer_long_3s, phis=outer_phis, sides=outer_sides, chi2s=outer_chi2s, model_scales=outer_model_scales, /router_torus, plasma_rs=outer_plasma_rs, _EXTRA=extra
  
  ;; Floor of outer_ndays
  iouter_ndays = floor(outer_ndays)
  outer_idx = uniq(iouter_ndays, sort(iouter_ndays))
  outer_h = histogram(outer_model_scales[outer_idx], binsize=0.5, locations=outer_locations)
  outer_h_err = sqrt(outer_h)

  outer_low_idx = where(outer_model_scales[outer_idx] lt 3.5, complement=outer_high_idx, count, ncomplement=high_count)
  if count gt 0 then begin
     outer_low_idx = outer_idx[outer_low_idx]
     print, 'Ndays with low outer norm value: ', iouter_ndays[outer_low_idx]
     print, 'Dates with low outer norm value: ', nday2date(iouter_ndays[outer_low_idx])
  endif
  if high_count gt 0 then begin
     outer_high_idx = outer_idx[outer_high_idx]
     print, 'Ndays with high outer norm value: ', iouter_ndays[outer_high_idx]
     print, 'Dates with high outer norm value: ', nday2date(iouter_ndays[outer_high_idx])
     print, 'Scale factors: ', outer_model_scales[outer_high_idx]
  endif

  ;; Find days for which we have both inner and outer.  Be careful
  ;; because the outer_ and inner_idx are into the floored versions
  if NOT keyword_set(sysIV) then $
     sysIV = 10.21
  print, 'Printing table for paper'
  for inday=0,N_elements(inner_idx)-1 do begin
     inner_outer_idx = where(iouter_ndays[outer_idx] eq iinner_ndays[inner_idx[inday]], count)
     if count eq 0 then $
        CONTINUE
     if count gt 1 then $
        message, 'ERROR: expecting one or no matches'

     ;; If we made it here, we have both inner and outer on this day.
     ;; Blossom back up to the idx from the output of the original
     ;; ssg_smyth_chi2 calls.
     ;; unwrap
     inner_outer_idx = outer_idx[inner_outer_idx]
     this_inday = iouter_ndays[inner_outer_idx]
     ;; this_inday is an array + where requires a scalar.
     this_inday = this_inday[0]
     this_outer_idx = where(iouter_ndays eq this_inday, nouter)
     this_inner_idx = where(iinner_ndays eq this_inday, ninner)

     ;; Print table of observations line-by-line
     which_first = 'O'
     if inner_ndays[this_inner_idx[0]] lt outer_ndays[this_outer_idx[0]] then $
        which_first = 'I'

     ;; Collect times for phase relationship searching
     pfo_array_append, begin_outers, outer_ndays[this_outer_idx[0]]
     pfo_array_append, end_outers, outer_ndays[this_outer_idx[nouter-1]]

     ;; Collect outer/inner ratio
     outer_inner_ratio = outer_model_scales[this_outer_idx[0]] / $
                         inner_model_scales[this_inner_idx[0]]
     ;; Prepare to print phase to previous measurement
     N_days = N_elements(end_outers)
     if N_days le 1 then begin
        min_diff = 0
        max_diff = 0
     endif else begin
        min_diff = (begin_outers[N_days-1] - end_outers[N_days-2]) * 24. / sysIV
        max_diff = (end_outers[N_days-1] - begin_outers[N_days-2]) * 24. / sysIV
     endelse
     print, string(format='(i6, 2(a20), 6(F6.1), A2, 2(F6.1))', $
                   iouter_ndays[this_outer_idx[0]], $
                   nday2date([outer_ndays[this_outer_idx[0]], $
                              outer_ndays[this_outer_idx[nouter-1]]]), $
                   outer_long_3s[this_outer_idx[0]], $
                   outer_long_3s[this_outer_idx[nouter-1]], $                   
                   outer_phis[this_outer_idx[0]], $
                   outer_phis[this_outer_idx[nouter-1]], $
                   outer_model_scales[this_outer_idx[0]], $
                   outer_inner_ratio, $
                   which_first, $
                   min_diff, $
                   max_diff)

     ;; Prepare for histogram of outer/inner scale factor ratios
     pfo_array_append, outer_inner_ratios, outer_inner_ratio

  endfor ;; each inner nday to find matches with outer

;;;;; Process beginning and end times of outer torus observations to
;;;;; see if we have any system III or IV phasing
;;;N_days = N_elements(end_outers)
;;;
;;;;; Do this at first for just adjacent measurements
;;;min_diffs = begin_outers[1:N_days-1] - end_outers[0:N_days-2]
;;;max_diffs = end_outers[1:N_days-1] - begin_outers[0:N_days-2]
;;;
;;;
;;;
;;;;; This might be overkill, since if we don't see a pattern in the next
;;;;; --> I am going to want to do all permutations and combinations
;;;;; Arrays of differences between begin and end points
;;;for inday=0, N_days-2 do begin
;;;   pfo_array_append, min_diffs, $
;;;                     begin_outers[inday+1:N_days-1] - end_outers[inday]
;;;   pfo_array_append, max_diffs, $
;;;                     end_outers[inday+1:N_days-1] - begin_outers[inday]
;;;   ;; Midpoint of each second observation with the same array
;;;   ;; structure (flat) as the *_diffs
;;;   pfo_array_append, outer_midpoints, $        
;;;                     replicate((end_outers[inday+1] + $
;;;                                begin_outers[inday+1]) / 2., $
;;;                               N_days-1-inday)
;;;end
;;;
;;;
;;;if NOT keyword_set(sysIV) then $
;;;   sysIV = 10.21
;;;min_sysIVs = min_diffs mod (sysIV * 24.)
;;;max_sysIVs = max_diffs mod (sysIV * 24.)
;;;
;;;min_sysIV_idx = uniq(min_sysIVs, sort(min_sysIVs))
;;;min_sysIV_h = histogram(min_sysIVs[min_sysIV_idx], binsize=0.5, locations=min_locations)
;;;max_sysIV_idx = uniq(max_sysIVs, sort(max_sysIVs))
;;;max_sysIV_h = histogram(max_sysIVs[max_sysIV_idx], binsize=0.5, locations=max_locations)
;;;
;;;plot, min_locations, min_sysIV_h, psym=!tok.hist
;;;oplot, max_locations, max_sysIV_h, psym=!tok.hist, color=!tok.green
;;;
;;;return
;;;n_diff_idx = uniq(min_diffs, sort(min_diffs))
;;;min_diff_h = histogram(min_diffs[min_diff_idx], binsize=0.5, locations=min_locations)
;;;max_diff_idx = uniq(max_diffs, sort(max_diffs))
;;;max_diff_h = histogram(max_diffs[max_diff_idx], binsize=0.5, locations=max_locations)
;;;
;;;plot, min_locations, min_diff_h, psym=!tok.hist
;;;oplot, max_locations, max_diff_h, psym=!tok.hist, color=!tok.green
;;;
;;;return

  ;; Make histogram plot
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
  
  ;; Make the outer/inner histogram look nice by using the inner/outer ratio
  h = histogram(1./outer_inner_ratios, binsize=0.5, locations=locations, reverse_indices=ridx)
  locations += (locations[1] - locations[0])/2.
  herr = sqrt(h)
  plot, 1./locations, h, psym=!tok.hist, $
        xtitle='Outer/Inner scale factor', ytitle='Histogram', xrange=[0.3,1]
  errplot, 1./locations, h-herr, h+herr


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

