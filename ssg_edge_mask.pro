;+
; $Id: ssg_edge_mask.pro,v 1.1 2003/03/10 18:28:24 jpmorgen Exp $

; ssg_edge_mask.  Make an edge mask for an image using information in
; the header

;-

function ssg_edge_mask, im, hdr

  sxaddhist, string('(ssg_edge_mask.pro) ', systime(/UTC), ' UT'), hdr
  sli_bot = strtrim(sxpar(hdr,'SLI_BOT',COUNT=count))
  if count eq 0 then begin
     message, 'WARNING: SLI_BOT keyword missing.  Try running ssg_[get & fit]_sliloc for better results', /CONTINUE
     sli_bot = 0
  endif
  sli_top = strtrim(sxpar(hdr,'SLI_TOP',COUNT=count))
  if count eq 0 then begin
     message, 'WARNING: SLI_TOP keyword missing.  Try running ssg_[get & fit]_sliloc for better results', /CONTINUE
     sli_top = ny-1
  endif
  
  ;; Make an edge mask to blot out pixels that would mess up the
  ;; dispersion spectrum.  Try the flatfield source image, if
  ;; available, otherwise from SLI_BOT and SLI_TOP
  lampdir = strtrim(sxpar(hdr,'LAMPDIR',COUNT=count))
  if count ne 0 then begin
     flatfile = lampdir + '/' + strtrim(sxpar(hdr,'LAMPSOUR',COUNT=count))
     if count eq 0 then message, 'WARNING: LAMPSOUR keyword missing even though LAMPDIR is present.  This shouldn''t happen, but for now I am just going to use the slicer top and bottom to do my work.  I suggest you fix/rerun ssg_flatfield', /CONTINUE
  endif
  if count ne 0 then begin
     flat=ssgread(flatfile, fhdr, /DATA, /TRIM)
     sxaddhist, '(ssg_edge_mask.pro) Using LAMPSOUR to define an edge mask ', hdr
     sim = ssgread(im, hdr, /DATA, /TRIM)
     asize=size(sim) & snx=asize[1] & sny=asize[2]
     temp = ssg_flat_align(sim, hdr, flat, fhdr)
     temp = temp * 0.
     edge_mask = im + !values.f_nan
     edge_mask[*, 0:sny-1] = temp[*,*]
     sxaddhist, '(ssg_edge_mask.pro) subimage discarded', hdr
  endif else begin
     message, 'NOTE: making edge mask from SLI_BOT and SLI_TOP instead of flatfield source image.'/CONTINUE              
     sxaddhist, '(ssg_edge_mask.pro) WARNING: Used SLI_BOT and SLI_TOP to define edge mask', hdr
     sxaddhist, '(ssg_edge_mask.pro) WARNING: use of flatfield is prefered', hdr
     edge_mask = im * 0
     edge_mask[0:sli_bot] = !values.f_nan
     edge_mask[sli_top:ny-1] = !values.f_nan
  endelse ;; Making an edge mask


  return, edge_mask
end
