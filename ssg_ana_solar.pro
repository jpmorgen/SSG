;+
; NAME: ssg_ana_solar
;
; PURPOSE: Analyze solar Fraunhofer data culled from ssg fits by ssg_fit2ana
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
pro ssg_ana_solar, fname
  init = {tok_sysvar}
  ;; Read back in fh_array which was saved in ssg_fit2ana.  This code
  ;; depends highly on that code.
  restore, fname
  fh0 = 12 ;; first Fraunhoffer line index
  nfhp = 9 ;; number of parameters to store per Fraunhofer line
  dims = size(fh_array, /dimensions)
  max_lines = (dims[1] - fh0) / nfhp ;; should be 100
  ;; Make indices into our rest wavelengths for one nday
  fh_rwl_idx1 = fh0 + nfhp * indgen(max_lines)
  
  
  ;; Make an index into all our rest wavelengths.  Start with 2D to
  ;; make things easy to line up.  Note that IDL is column major, in
  ;; other words, the 0th dimension varies the fastest
  fh_rwl_idx = lon64arr(dims[0], max_lines)
  nday_idx = indgen(dims[0])
  for ifh=0,max_lines-1 do begin
     fh_rwl_idx[*, ifh] = fh_rwl_idx1[ifh]*dims[0] + nday_idx
  endfor
  good_idx = where(finite(fh_array[fh_rwl_idx]))
  ;; unwrap
  good_idx = fh_rwl_idx[good_idx]
  u_fh_rwl_idx = uniq(fh_array[good_idx], sort(fh_array[good_idx]))
  ;; unwrap
  u_fh_rwl_idx = good_idx[u_fh_rwl_idx]
  print, fh_array[u_fh_rwl_idx]

  ;; Cycle through each line to plot quantities 
  for irwl=0,N_elements(u_fh_rwl_idx)-1 do begin
     ;; Get 1D indices into fh_array for this Fraunhofer line
     tfh_idx = where(fh_array[fh_rwl_idx] eq fh_array[u_fh_rwl_idx[irwl]], count)
     if count lt 2 then $
        CONTINUE
     ;; unwrap
     tfh_idx = fh_rwl_idx[tfh_idx]
     ;; Convert to 2D indices, again remembering IDL is column major
     ;; (index 0 varies fastest)
     param_idx = tfh_idx / dims[0]
     nday_idx = tfh_idx mod dims[0]

     erase
     ;; Create X-axes of the plot
     ag_axis = fh_array[nday_idx, param_idx+1] - 6300.304
     io_axis = fh_array[nday_idx, param_idx+1] - fh_array[nday_idx, 1]
     ;;io_axis = reform(io_axis, N_elements(io_axis))
     !p.multi = [2, 0, 2]
     center = median(io_axis)
     plot, io_axis, fh_array[nday_idx, param_idx+3], psym=!tok.plus, $
           xtitle='Delta lambda (A) from Io line', xrange=[center-1, center+1], $
           ytitle='Equivalent Width', xstyle=!tok.exact
     oploterr, io_axis, fh_array[nday_idx, param_idx+3], fh_array[nday_idx, param_idx+4], !tok.dot
     plot, io_axis, fh_array[nday_idx, param_idx+5], psym=!tok.diamond, $
           xtitle='Delta lambda (A) from Io line', xrange=[center-1, center+1], $
           ytitle='Doppler Width', xstyle=!tok.exact
     oploterr, io_axis, fh_array[nday_idx, param_idx+5], fh_array[nday_idx, param_idx+6], !tok.dot
     wait, 1
  endfor

end


;;     wait, 1
;;     good_idx = where(finite(fh_array[tfh_idx+5]), count)
;;     if count lt 2 then $
;;        CONTINUE
;;
;;          endif else begin
;;        ;; Keep plot window functions the same
;;        plot, [0], [0]
;;     endelse
