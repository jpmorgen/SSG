;+
; $Id: ssg_flat_align.pro,v 1.2 2003/03/10 18:30:50 jpmorgen Exp $

; ssg_flat_align  Rotate and translate a flatfield image to match up
; with an image.  Move the flatfield image so that sharp features in
; the image to be processed, like cosmic ray hits, do not get smeared
; out.

;-

function ssg_flat_align, im, hdr, in_flat, fhdr, ref_pix=ref_pix, quiet=quiet

  ON_ERROR, 2
  flat = in_flat

  if N_elements(flat) ne N_elements(im) then $
    message, 'ERROR: input images are not the same size.  Did you specify flatfile= on the command line?'

  asize=size(im) & nx=asize[1] & ny=asize[2]
  if NOT keyword_set(ref_pix) then ref_pix = nx/2.


  ;; Deal with flatfield header.

  ;; Get a name for the flatfield file
  flatfile = sxpar(fhdr, 'SSGFILE', count=count)
  if count eq 0 then $
    flatfile = 'UNKNOWN'

  ;; SLI_CENT
  flat_sli_cent = sxpar(fhdr, 'SLI_CENT', count=count)
  if count eq 0 then begin
     flat_sli_cent = ny/2.
     message, 'WARNING: '+ flatfile + ' does not have a SLI_CENT keyword.  Did you run ssg_get_slicent?  Continuing using image center, ' + string(flat_sli_cent), /CONTINUE
  endif

  ;; CAM_ROT
  flat_rot = sxpar(fhdr, 'CAM_ROT', count=count)
  if count eq 0 then message, 'ERROR: ' + flatfile + ' does not have a CAM_ROT keyword.  Did you run ssg_get_camrot?'

  ;; Deal with image header

  ;; SLI_CENT
  sli_cent = sxpar(hdr, 'SLI_CENT', count=count)
  if count eq 0 then begin
     message, 'WARNING: file '+ files[i] + ' does not have a SLI_CENT keyword.  You should run ssg_[get&fit]_slicent stuff first', /CONTINUE
     sli_cent = ny/2.
  endif

  ;; CAM_ROT
  cam_rot = sxpar(hdr, 'CAM_ROT', count=count)
  if count eq 0 then message, 'ERROR: file '+ files[i] + ' does not have a CAM_ROT keyword.  You need to run ssg_[get&fit]_camrot stuff first'
  angle = cam_rot-flat_rot
  if flat_rot ne cam_rot then begin
     flat=ssg_camrot(flat, angle, ref_pix, flat_sli_cent)
     if NOT keyword_set(quiet) then $
       message, /INFORMATIONAL, 'Rotating ' + flatfile + ' by ' + string(angle) + ' to line up with input image'
     sxaddhist, string(format='("(ssg_flat_align.pro) rotated ", a, " by ", f6.2, " degrees")', flatfile, angle), hdr
  endif

  ;; Do translation if we need to
  if flat_sli_cent ne sli_cent then begin
     flat=ssg_camrot(flat, 0, ref_pix, ny/2. - sli_cent + flat_sli_cent, /NOPIVOT)
     if NOT keyword_set(quiet) then $
       message, 'Translating ' + flatfile + ' by ' + string(sli_cent-flat_sli_cent), /CONTINUE
     sxaddhist, string(format='("(ssg_flat_align.pro) translating ", a, " by ", f6.2, " pixels")', flatfile, sli_cent-flat_sli_cent), hdr
  endif

  return, flat

end
