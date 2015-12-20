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
function ssg_column_replace, im, hdr, map, nbad

  ;; Default map is no NANs
  map = im*0.
  ;; Collapse into 1D
  ssg_spec_extract, im, hdr, spec, xdisp, /AVERAGE
  ;; Find bad columns using 1D spectrum
  bad_spec_idx = where(finite(spec) eq 0, nbad, complement=good_spec_idx)
  ;; Return if none found
  if nbad eq 0 then $
     return, im

  ;; If we made it here, we have some bad columns
  asize=size(im) & nx=asize[1] & ny=asize[2]
  sim = im
  ;; Go through row by row using an iterative approach to fill in the
  ;; NAN pixels
  for iy=0,ny-1 do begin
        bad_idx = where(finite(sim[*, iy]) eq 0, count, complement=good_idx)
     ;; Check for NANed out rows, which can happen when there is a big
     ;; sli_cent shift
     if count eq nx then $
        CONTINUE
     ;; Start with a small smooth window width and grow out from there
     width = 1
     while count gt 0 and width lt nx do begin
        osim = sim
        sim[*, iy] = smooth(sim[*,iy], width, /NAN)
        ;; Make sure we always use the original or first smoothed
        ;; version, since multiple smooths seem to bump values up
        ;; unexpectly
        sim[good_idx, iy] = osim[good_idx, iy] 
        width += 1
        bad_idx = where(finite(sim[*, iy]) eq 0, count, complement=good_idx)
     endwhile 
     ;; Try just smoothing once over that maximum smooth interval
     ;;sim[*, iy] = smooth(im[*,iy], width, /NAN)
  endfor
  ;; Now put original non-NAN pixels into sim
  good_idx = where(finite(im), count)
  if count eq 0 then $
     message, 'ERROR: there should be at least some good pixels'
  sim[good_idx] = im[good_idx]

  ;; Now prepare a mask that is slightly larger than the area of the
  ;; original NANed-out columns and is arranged in clean columns.  THe
  ;; mask is going to be applied multiplicatively, so we want the good
  ;; pixels to end up being 1s and NANs still NAN
  spec[bad_spec_idx] = 0
  spec[good_spec_idx] = 1
  ;; Run a very narrow smooth along spec to pull edges of good areas
  ;; away from 1 to prevent "glips" after spectra are rotated and morphed
  spec = smooth(spec, 3)
  ;; Set anyting that isn't a clean 1 to NAN
  spec[where(spec lt 1)] = !values.F_NAN
  ;; Turn that back into a 2D image
  for ix=0,nx-1 do begin
     map[ix, *] = spec[ix]     
  endfor

  return, sim
end

;This procedure will replace all the bad columns with non-nan values for rotation.
;It will also generate a file that will hold a mapping of all the the nan values so that
;when derotation is complete we can replace all the nan values. We don't want to have
;fabricated data in our final analysis.

;;
;;imSize = SIZE(im)
;;XElements = imSize[1]
;;YElements = imSize[2]
;;IF (imSize[0] EQ 1) THEN middle = 0 ELSE middle = ROUND(YElements/2)
;;map = MAKE_ARRAY(XElements, YElements, value=0)
;;firstRow = 1
;;SegmentCounter = 1
;;firstFinite = 1
;;
;;for i=0, XElements-1 DO BEGIN
;;
;;	IF (finite(im[i, middle]) EQ 0 and i EQ 0) THEN BEGIN
;;		map[i, *] = 1
;;		firstFinite = 0
;;		CONTINUE
;;	ENDIF
;;	IF (finite(im[i, middle]) EQ 0 and firstFinite EQ 0) THEN BEGIN
;;		map[i, *] = 1
;;		CONTINUE
;;	ENDIF
;;	IF (finite(im[i, middle]) EQ 1 and firstFinite EQ 0) THEN BEGIN
;;		map[i, *] = 0
;;		firstFinite = 1
;;		CONTINUE
;;	ENDIF
;;	IF (finite(im[i, middle]) EQ 0 and firstFinite EQ 1) THEN BEGIN
;;		map[i, *] = 1
;;		FirstRow = 0
;;		SegmentCounter = SegmentCounter+1
;;		CONTINUE
;;		 
;;	ENDIF ELSE BEGIN
;;		map[i, *] = 0 	
;;		IF(firstrow EQ 0) THEN BEGIN
;;			SegmentSize = SegmentCounter+1
;;			imSegment = MAKE_ARRAY(SegmentSize, YElements)
;;			FOR j=1, segmentSize DO BEGIN
;;				imSegment[segmentSize-J, *] = im[i+1-j, *]
;;			ENDFOR
;;			segmentToInsert = SMOOTH(imSegment, [segmentSize-1, 1], /NAN)
;;			FOR j=1, segmentSize DO BEGIN
;;				im[i+1-j, *] = segmentToInsert[segmentSize-j, *]
;;			ENDFOR
;;			firstRow = 1
;;			SegmentCounter = 1
;;		ENDIF
;;		
;;	ENDELSE
;;ENDFOR
;;
;;END
