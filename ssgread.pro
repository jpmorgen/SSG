; $Id: ssgread.pro,v 1.2 2002/11/12 21:31:34 jpmorgen Exp $

; sshread uses ccdread to get an Stellar SpectroGraph image and
; rotates it to the correct orientation for easy spectral extraction
; IMPORTANT: if you are going to write this image out, make sure you
; use the hdr supplied by this routine!  filename can be the image if
; you have already read the image in with another routine

function ssgread, fname_or_im, hdr, TV=tv, REUSE=reuse, zoom=zoom, VERBOSE=verbose, data=data, bias=bias

  ON_ERROR, 2
  silent = 1
  if keyword_set(verbose) then silent = 0

  if N_elements(fname_or_im) eq 0 then message, 'ERROR: no filename or image supplied'
  if size(fname_or_im, /TNAME) eq 'STRING' then begin
     ;; fname_or_im is really a file name, not the image
     filename = strtrim(fname_or_im)
     im = ccdread(filename, hdr, SILENT=silent) ; Returns im as a float array
  endif else begin
     ;; filename is the image, we are probably being called by
     ;; ssg_raw_cp
     im = float(fname_or_im)
     if N_elements(hdr) eq 0 then $
       sxaddpar, hdr, 'RAWFILE', 'UNKNOWN'
     filename = sxpar(hdr, 'RAWFILE', COUNT=count)
     if count eq 0 then filename = 'UNKNOWN'
  endelse

  if N_elements(size(im, /DIMENSIONS)) ne 2 then message, 'ERROR: specify a valid FITS filename or a 2D array.'

  asize=size(im)
  nx=asize[1]
  ny=asize[2]

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
  err = 0
  CATCH, err
  if err ne 0 then begin
     message, /NONAME, !error_state.msg, /CONTINUE
     message, 'there seem to be no BIASSEC, etc. keywords in this header.  Roting the image 90 degree clockwise, but skipping the header modifications',/CONTINUE
     sxaddhist, string('(ssgread.pro) Error manipulating BIASSEC, etc. keywords.'), hdr
     sxaddhist, string('(ssgread.pro) rotated image 90 degrees clockwise hoping'), hdr 
     sxaddhist, string('(ssgread.pro) blue ends up left, slice 1 on the bottom'), hdr

  endif else begin
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
  endelse
  CATCH, /CANCEL

  ;; If /DATA, get non-overclock region 
  if keyword_set (data) then begin
     datasec = strtrim(sxpar(hdr,'DATASEC',COUNT=count))
     if count eq 0 then begin
        message,'WARNING: no DATASEC keyword in FITS header, using whole image', /CONTINUE
        data_im=im
     endif else begin
        toks=strsplit(datasec,'[:,]')
        if N_elements(toks) ne 4 then begin
           message, 'ERROR: unknown DATASEC string format, using whole image', $
                    /CONTINUE
           data_im=im
        endif else begin
           coords=strsplit(datasec,'[:,]',/extract)
           coords = coords - 1  ; Translate into IDL array reference
           data_im = im[coords[0]:coords[1],coords[2]:coords[3]]
        endelse
     endelse
     im = data_im
  endif

  ;; If /BIAS, get non-overclock region 
  if keyword_set (bias) then begin
     biassec = strtrim(sxpar(hdr,'BIASSEC',COUNT=count))
     if count eq 0 then begin
        message,'WARNING: no BIASSEC keyword in FITS header, using whole image', /CONTINUE
        bias_im=im
     endif else begin
        toks=strsplit(biassec,'[:,]')
        if N_elements(toks) ne 4 then begin
           message, 'ERROR: unknown BIASSEC string format, using whole image', $
                    /CONTINUE
           bias_im=im
        endif else begin
           coords=strsplit(biassec,'[:,]',/extract)
           coords = coords - 1  ; Translate into IDL array reference
           bias_im = im[coords[0]:coords[1],coords[2]:coords[3]]
        endelse
     endelse
     im = bias_im
  endif

  if keyword_set(tv) then begin
     display, im, title=filename, REUSE=reuse, zoom=zoom, rotate=rotate
  endif

  return,im
end

