;+
; $Id: ssg_edge_mask.pro,v 1.2 2003/06/11 18:13:01 jpmorgen Exp $

; ssg_edge_mask.  Make an edge mask for an image using information in
; the header.  Edge_cut = optional.  It is the cut value in the
; normalized flatfield that defines the edges.  FLAT_CUT is used if
; edge_cut is missing.

;-

function ssg_edge_mask, im, hdr, edge_cut

  asize=size(im) & nx=asize[1] & ny=asize[2]
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
     ;; Flatfield is a trimmed data image, so work with a sub-image
     shdr=hdr
     sim = ssgread(im, shdr, /DATA, /TRIM)
     asize=size(sim) & snx=asize[1] & sny=asize[2]
     flat = ssg_flat_align(sim, shdr, flat, fhdr)
     edge_idx = where(normalize(flat, edge_cut) lt edge_cut, $
                      count, complement=middle_idx)
     ;; flat is a handy array, so nuke the edge pixels and set the
     ;; rest to 0
     if count gt 0 then begin
        flat[edge_idx] = !values.f_nan
     endif
     flat = flat * 0.

     ;; Now insert into the larger image
     edge_mask = im + !values.f_nan
     edge_mask[0:snx-1, 0:sny-1] = flat[*,*]
  endif else begin
     message, 'NOTE: making edge mask from SLI_BOT and SLI_TOP instead of flatfield slicer image.', /CONTINUE              
     sxaddhist, '(ssg_edge_mask.pro) WARNING: Used SLI_BOT and SLI_TOP to define edge mask', hdr
     sxaddhist, '(ssg_edge_mask.pro) WARNING: use of flatfield is prefered', hdr
     edge_mask = fltarr(nx,ny)
     edge_mask[*,0:sli_bot] = !values.f_nan
     edge_mask[*,sli_top:ny-1] = !values.f_nan
  endelse ;; Making an edge mask


  return, edge_mask
end
