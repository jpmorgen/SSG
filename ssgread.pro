; $Id: ssgread.pro,v 1.1 2002/10/28 17:37:39 jpmorgen Exp $

; sshread uses ccdread to get an Stellar SpectroGraph image and
; rotates it to the correct orientation for easy spectral extraction
; IMPORTANT: if you are going to write this image out, make sure you
; use the hdr supplied by this routine!  filename can be the image if
; you have already read the image in with another routine

function ssgread, filename, hdr, TV=tv, REUSE=reuse, zoom=zoom, VERBOSE=verbose

  ON_ERROR, 2
  silent = 1
  if keyword_set(verbose) then silent = 0

  if N_elements(filename) eq 0 then message, 'ERROR: no filename or image supplied'
  if size(filename, /TNAME) eq 'STRING' then begin
     ;; filename is really a file name, not the image
     filename = strtrim(filename)
     im = ccdread(filename, hdr, SILENT=silent) ; Returns im as a float array
  endif else begin
     ;; filename is the image, we are probably being called by
     ;; ssg_raw_cp
     im = float(filename)
     filename = sxpar(hdr, 'RAWFILE', COUNT=count)
     if count eq 0 then filename = 'UNKNOWN'
  endelse

  nx=sxpar(hdr,'NAXIS1')
  ny=sxpar(hdr,'NAXIS2')

  sxaddhist, string('(ssgread.pro) ', systime(/UTC), ' UT'), hdr
  cd,current=cwd
  temp = sxpar(hdr, 'PARENTH', COUNT=count)
  if count eq 0 then $
    sxaddpar, hdr, 'PARENTH', getenv("HOST"), ' hostname'
  temp = sxpar(hdr, 'PARENTD', COUNT=count)
  if count eq 0 then $
    sxaddpar, hdr, 'PARENTD', cwd, ' current working directory'
  temp = sxpar(hdr, 'PARENT', COUNT=count)
  if count eq 0 then $
    sxaddpar, hdr, 'PARENT', filename, ' parent file'

  ;; Flip so blue is to the left, slice 1 is down
  if nx lt ny then begin
     im=rotate(im,1)
     sxaddpar, hdr, 'NAXIS1', ny, 'size of axis 1'
     sxaddpar, hdr, 'NAXIS2', nx, 'size of axis 2'
     sxaddhist, string('(ssgread.pro) rotated image so blue is left, slice 1 is bottom'), hdr
     sxaddpar, hdr, 'BIASSEC', transpose_array_string(sxpar(hdr,'BIASSEC'))
     sxaddpar, hdr, 'TRIMSEC', transpose_array_string(sxpar(hdr,'TRIMSEC'))
     sxaddpar, hdr, 'DATASEC', transpose_array_string(sxpar(hdr,'DATASEC'))
     sxaddpar, hdr, 'CCDSEC' , transpose_array_string(sxpar(hdr,'CCDSEC'))
     sxaddpar, hdr, 'ORIGSEC', transpose_array_string(sxpar(hdr,'ORIGSEC'))
     sxaddhist, string('(ssgread.pro) modified *SEC keywords to reflect rotation'), hdr
  endif else $
    sxaddhist, string('(ssgread.pro) image already had blue left, slice 1 bottom'), hdr

  if keyword_set(tv) then begin
     display, im, title=filename, REUSE=reuse, zoom=zoom, rotate=rotate
  endif

  return,im
end

