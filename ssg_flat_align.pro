;+
; $Id: ssg_flat_align.pro,v 1.1 2002/12/16 13:41:54 jpmorgen Exp $

; ssg_flatfield Divide comp and object images by flatfield, recording
; flatfield name in database

;-

function ssg_flat_align, im, hdr, in_flat, fhdr, ref_pix=ref_pix, quiet=quiet

  flat = in_flat

  if N_elements(flat) ne N_elements(im) then $
    message, 'ERROR: "' + flat_fnames[i] + '" and "' + files[i] + '" are not the same size.  Did you specify flatfile= on the command line?'


  asize=size(im) & nx=asize[1] & ny=asize[2]
  if NOT keyword_set(ref_pix) then ref_pix = nx/2.

  ;; Rotate 
  flat_sli_cent = sxpar(fhdr, 'SLI_CENT', count=count)
  if count eq 0 then begin
     message, 'WARNING: flatfield file '+ flat_fnames[i] + ' does not have a SLI_CENT keyword.  Did you use ssg_flatgen to create it?', /CONTINUE
     flat_sli_cent = ny/2.
  endif

  flat_rot = sxpar(fhdr, 'CAM_ROT', count=count)
  if count eq 0 then message, 'ERROR: flatfield file '+ flat_fnames[i] + ' does not have a CAM_ROT keyword.  Did you use ssg_flatgen to create it?'


  ;; Deal with image header
  sli_cent = sxpar(hdr, 'SLI_CENT', count=count)
  if count eq 0 then begin
     message, 'WARNING: file '+ files[i] + ' does not have a SLI_CENT keyword.  You should run ssg_[get&fit]_slicent stuff first', /CONTINUE
     sli_cent = ny/2.
  endif
  cam_rot = sxpar(hdr, 'CAM_ROT', count=count)
  if count eq 0 then message, 'ERROR: file '+ files[i] + ' does not have a CAM_ROT keyword.  You need to run ssg_[get&fit]_camrot stuff first'
  if flat_rot ne cam_rot then begin
     flat=rot(flat, cam_rot-flat_rot, /PIVOT, $
              1., ref_pix, flat_sli_cent, $
              cubic=-0.5, missing = !values.f_nan)
     if NOT keyword_set(quiet) then $
       message, /INFORMATIONAL, 'Rotating FLATFILE by ' + string(cam_rot-flat_rot) + ' to line up with input image'
     sxaddhist, string(format='("(ssg_flatfield.pro) rotated FLATFILE by ", f6.2, " degrees")', cam_rot-flat_rot), hdr
  endif

  ;; Do translation if we need to
  if flat_sli_cent ne sli_cent then begin
     flat=rot(flat, 0, $
              1., ref_pix, ny/2. - sli_cent + flat_sli_cent, $
              cubic=-0.5)
     message, 'WARNING: flatfield and image do not have the same slicer center.  Traslating flatfield by ' + string(sli_cent-flat_sli_cent), /CONTINUE
     sxaddhist, string(format='("(ssg_flatfield.pro) translating FLATFILE by ", f6.2, " pixels")', sli_cent-flat_sli_cent), hdr
  endif

  return, flat

end
