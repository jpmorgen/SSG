;+
; NAME: ssg_column_replace.pro

; PURPOSE: replace NANs which mark bad columns with some sort of
; reasonable values so that rot and slicer morphs won't make NAN
; patches grow.  Save off slightly larger map of replaced values so
; they can be replaced with NANs afterward

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
function ssg_column_replace, im, mask, nbad, grow_mask=grow_mask

  ;; Default mask is no NANs
  mask = im*0.
  asize=size(im) & nx=asize[1] & ny=asize[2]

  ;; Collapse into 1D.  Don't do this with ssg_spec_extract,
  ;; since we would need hdr
  spec = make_array(nx, value=!values.f_NAN)
  for si=0,nx-1 do begin
     good_idx = where(finite(im[si,*]) eq 1, count)
     if count gt 0 then begin
        spec[si] = total(im[si,good_idx])/count
     endif
  endfor

  ;; Find bad columns using 1D spectrum
  bad_spec_idx = where(finite(spec) eq 0, nbad, complement=good_spec_idx)
  ;; Return if none found or if everything is bad
  if nbad eq 0 or nbad eq nx then $
     return, im

  ;; If we made it here, we have some bad columns
  sim = im ;; smoothed image
  ;; Go through row by row using an iterative approach to fill in the
  ;; NAN pixels
  for iy=0,ny-1 do begin
     first_bad_idx = where(finite(sim[*, iy]) eq 0, count, complement=good_idx)
     ;; Check for NANed out rows, which can happen when there is a big
     ;; sli_cent shift
     if count eq nx then $
        CONTINUE
     ;; Start with a small smooth window width and grow out from there
     width = 1
     while count gt 0 and width lt nx do begin
        osim = sim
        sim[*, iy] = smooth(sim[*,iy], width, /NAN, /edge_mirror)
        ;; Make sure we always use the original or first smoothed
        ;; version, since multiple smooths seem to bump values up
        ;; unexpectly
        sim[good_idx, iy] = osim[good_idx, iy]
        ;; Try to go a little faster
        width += 3
        bad_idx = where(finite(sim[*, iy]) eq 0, count, complement=good_idx)
     endwhile 
     ;; Try just smoothing once over that maximum smooth interval
     ;; This didn't work well because it erased small NAN segments
     ;;sim[*, iy] = smooth(im[*,iy], width, /NAN)

     ;; We need to put NANs which weren't from bad columns back in.
     ;; Loop through all our original NANs in this row and compare to
     ;; the NANs we spotted in our collapsed dispersion spectrum
     for ibad=0, nbad-1 do begin
        junk = where(bad_spec_idx[ibad] eq first_bad_idx, hit)
        if hit gt 0 then $
           sim[first_bad_idx[ibad]] = !values.f_NAN
     endfor ;; each 
     
  endfor ;; each cross-dispersion row

  ;; Now put original non-NAN pixels into sim
  good_idx = where(finite(im), count)
  if count eq 0 then stop;;$
  ;;message, 'ERROR: there should be at least some good pixels'
  sim[good_idx] = im[good_idx]


  ;; Now prepare a mask that is slightly larger than the area of the
  ;; original NANed-out columns and is arranged in clean columns.  THe
  ;; mask is going to be applied multiplicatively, so we want the good
  ;; pixels to end up being 1s and NANs still NAN
  spec[bad_spec_idx] = 0
  spec[good_spec_idx] = 1
  ;; If desired, run a very narrow smooth along spec to pull edges of
  ;; good areas away from 1 to prevent "glips" after spectra are
  ;; rotated and morphed
  if keyword_set(grow_mask) then $
     spec = smooth(spec, grow_mask, /edge_mirror)
  ;; Set anyting that isn't a clean 1 to NAN
  spec[where(spec lt 1)] = !values.f_NAN
  ;; Turn that back into a 2D image
  for ix=0,nx-1 do begin
     mask[ix, *] = spec[ix]     
  endfor

  return, sim
end

