;+
; $Id: ssg_edge_mask.pro,v 1.2 2003/06/11 18:13:01 jpmorgen Exp $

; ssg_edge_mask.  Make an edge mask for an image using information in
; the header.  Edge_cut = optional.  It is the cut value in the
; normalized flatfield that defines the edges.  FLAT_CUT is used if
; edge_cut is missing.

;-

function ssg_edge_mask, im, hdr, edge_cut, $
                        bot_cut=bot_cut, $
                        top_cut=top_cut, $
                        _EXTRA=extra
  

  asize=size(im) & nx=asize[1] & ny=asize[2]
  sxaddhist, string('(ssg_edge_mask.pro) ', systime(/UTC), ' UT'), hdr
  sli_bot = sxpar(hdr,'SLI_BOT',COUNT=count)
  if count eq 0 then begin
     message, 'WARNING: SLI_BOT keyword missing.  Try running ssg_[get & fit]_sliloc for better results', /CONTINUE
     sli_bot = 0
  endif
  sli_top = sxpar(hdr,'SLI_TOP',COUNT=count)
  if count eq 0 then begin
     message, 'WARNING: SLI_TOP keyword missing.  Try running ssg_[get & fit]_sliloc for better results', /CONTINUE
     sli_top = ny-1
  endif
  
  ;; Make an edge mask to blot out pixels that would mess up the
  ;; dispersion spectrum.  Use the slicer image with appropriate
  ;; normalization, if available, otherwise from SLI_BOT and SLI_TOP
  lampdir = strtrim(sxpar(hdr,'LAMPDIR',COUNT=count))
  if count ne 0 then begin
     flatfile = lampdir + '/' + strtrim(sxpar(hdr,'LAMPSLIC',COUNT=count))
     if count eq 0 then message, 'WARNING: LAMPSLIC keyword missing even though LAMPDIR is present.  This shouldn''t happen, but for now I am just going to use the slicer top and bottom to do my work.  I suggest you fix/rerun ssg_flatfield', /CONTINUE
  endif
  if count ne 0 then begin
     flat=ssgread(flatfile, fhdr, /DATA, /TRIM)
     sxaddhist, '(ssg_edge_mask.pro) Using LAMPSLIC and EDGE_CUT to define an edge mask ', hdr
     if NOT keyword_set(edge_cut) then begin
        edge_cut = sxpar(hdr, 'FLAT_CUT', count=count)
        if count eq 0 then message, 'ERROR: FLAT_CUT keyword missing in header.  Try specifying flat_cut on the command line'
     endif
     sxaddpar, hdr, 'EDGE_CUT', edge_cut, ' cut in LAMP_SLIC for defining edge mask'

     ;; Rather than using the same edge_cut for the whole image, which
     ;; results in NANing deep slicer valleys and cutting off low
     ;; slice values when things aren't nicely aligned, work
     ;; carefully on each edge to find the best place to cut the image
     
     ;; Flatfield is a trimmed data image, so work with a sub-image
     shdr=hdr
     sim = ssgread(im, shdr, /DATA, /TRIM)
     asize=size(sim) & snx=asize[1] & sny=asize[2]
     flat = ssg_flat_align(sim, shdr, flat, fhdr)
     ;; Get a cross dispersion spectrum to work with
     ssg_spec_extract, flat, hdr, med_xdisp=med_xdisp, /AVERAGE

     ;; Find tops of slices 1 and 10.  This wasn't as easy as I would
     ;; have liked, since sometimes sli_bot and sli_top aren't that
     ;; great.  This is more of a statement of problems aligning flats
     ;; and object spectra than the flats themselves
     climbFrom = marks_edge_find(med_xdisp, /left,  /Deriv1)
     sli1_idx  = round(marks_edge_find(med_xdisp, /left,  /value, climbFrom=climbFrom))
     climbFrom = marks_edge_find(med_xdisp, /right,  /Deriv1)
     sli10_idx = round(marks_edge_find(med_xdisp, /right, /value, climbFrom=climbFrom))
     ;; Do some sanity checking
     if sli1_idx - sli_bot gt sny/10.*5 then begin
        message, 'WARNING: sli1_idx = ' + strtrim(sli1_idx, 2) + ' seems too high, setting to sli_bot + 2', /CONTINUE
        sli1_idx = sli_bot + 2
     endif
     if sli_top - sli10_idx gt sny/10.*5 then begin
        message, 'WARNING: sli10_idx = ' + strtrim(sli10_idx, 2) + ' seems too low, setting to sli_top - 2', /CONTINUE
        sli1_idx = sli_bot - 2
     endif
     
     ;; Find the value that corresponds to edge cut relative to the
     ;; tops of the slices for the edges (outside the primary spectral
     ;; region).  Do this in 1D for med_xdisp of the flat
     bot_cut = 0
     if sli1_idx gt 0 then begin
        temp = med_xdisp[0:sli1_idx]
        edge_idx = where(normalize(temp, edge_cut, _EXTRA=extra) $
                         lt edge_cut, $
                         count)
        if count gt 0 then $
           bot_cut = max(edge_idx)
     endif
     top_cut = sny-1
     if sli10_idx lt sny-1 then begin
        temp = med_xdisp[sli10_idx:sny-1]
        edge_idx = where(normalize(temp, edge_cut, _EXTRA=extra) $
                         lt edge_cut, $
                         count)
        if count gt 0 then $
           top_cut = min(edge_idx) + sli10_idx
     endif
     flat[*, 0:bot_cut] = !values.f_nan
     flat[*, top_cut:sny-1] = !values.f_nan

     ;;;;; Find the value that corresponds to edge cut relative to the
     ;;;;; tops of the slices for the edges (outside the primary spectral
     ;;;;; region).  This is in 2D, which might result in ragged edges
     ;;;temp = flat[*, 0:sli1_idx]
     ;;;edge_idx = where(normalize(temp, edge_cut) lt edge_cut, $
     ;;;                 count, complement=middle_idx)
     ;;;if count gt 0 then begin
     ;;;   temp[edge_idx] = !values.f_nan
     ;;;   flat[*, 0:sli1_idx] = temp
     ;;;endif
     ;;;temp = flat[*, sli10_idx:sny-1]
     ;;;edge_idx = where(normalize(temp, edge_cut) lt edge_cut, $
     ;;;                 count, complement=middle_idx)
     ;;;if count gt 0 then begin
     ;;;   temp[edge_idx] = !values.f_nan
     ;;;   flat[*, sli10_idx:sny-1] = temp
     ;;;endif

     ;; flat will be converted into an additive mask
     flat = flat * 0.

     ;; Now insert into the larger image
     edge_mask = im + !values.f_nan
     edge_mask[0:snx-1, 0:sny-1] = flat[*,*]
  endif else begin
     message, 'NOTE: making edge mask from SLI_BOT and SLI_TOP instead of flatfield slicer image.', /CONTINUE              
     sxaddhist, '(ssg_edge_mask.pro) WARNING: Used SLI_BOT and SLI_TOP to define edge mask', hdr
     sxaddhist, '(ssg_edge_mask.pro) WARNING: use of flatfield is preferred', hdr
     edge_mask = fltarr(nx,ny)
     edge_mask[*,0:sli_bot] = !values.f_nan
     edge_mask[*,sli_top:ny-1] = !values.f_nan
  endelse ;; Making an edge mask


  return, edge_mask
end
