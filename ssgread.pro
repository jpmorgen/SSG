; $Id: ssgread.pro,v 1.4 2003/03/10 18:32:57 jpmorgen Exp $

; sshread uses ccdread to get an Stellar SpectroGraph image and
; rotates it to the correct orientation for easy spectral extraction
; IMPORTANT: if you are going to write this image out, make sure you
; use the hdr supplied by this routine!  filename can be the image if
; you have already read the image in with another routine

function ssgread, fname_or_im, hdr, eim, ehdr, TV=tv, REUSE=reuse, zoom=zoom, VERBOSE=verbose, data=data, bias=bias, trim=trim

;  ON_ERROR, 2
  silent = 1
  if keyword_set(verbose) then silent = 0
  if N_elements(fname_or_im) eq 0 then message, 'ERROR: no filename or image supplied'

  if size(fname_or_im, /TNAME) eq 'STRING' then begin

     ;; fname_or_im is really a file name, not the image
     filename = strtrim(fname_or_im)
     im = ccdread(filename, hdr, SILENT=silent) ; Returns im as a float array
     
     ;; ERROR extension.  This should already be written as a float array
     temp = sxpar(hdr, 'EXTEND', COUNT=extend)
     if keyword_set(extend) then $
       eim = readfits(filename, ehdr, exten_no=1, SILENT=silent)

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

  asize=size(im) & nx=asize[1] & ny=asize[2]

  ;; Don't bother with detailed testing if we have been here before.
  ;; PARENTH is my own invention
  test = sxpar(hdr, 'PARENTH', COUNT=count)
  if count eq 0 then begin
     sxaddhist, string('(ssgread.pro) ', systime(/UTC), ' UT'), hdr

     ;; Generate some messages if we don't appear to be a normal SSG file.
     err = 0
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'this image is missing some header keywords that would identify it as a McMath-Pierce Stellar Spectrograph CCD image.',/CONTINUE
        sxaddhist, string('(ssgread.pro) WARNING: File is missing important SSG keywords'), hdr
        sxaddpar, hdr, 'NOT_SSG', '', 'keywords unique to the SSG are missing'
     endif else begin
        ;; This code is somewhat duplicated in ssg_fix_head
        origin = sxpar(hdr, 'ORIGIN')
        if strtrim(origin) ne 'KPNO-IRAF' then $
          message, 'WARNING: ORIGIN keyword is ' + string(origin) + ' not KPNO-IRAF'
        test = sxpar(hdr, 'IRAFNAME', count=count)
        if count eq 0 then $
          message, 'WARNING: No IRAFNAME keyword.'
        observat = sxpar(hdr, 'OBSERVAT')
        if strtrim(observat) ne 'NSO' then $
          message, 'WARNING: OBSERVAT keyword is ' + string(observat) + ' not NSO'
        test = sxpar(hdr, 'PROPID', count=count)
        if count eq 0 then $
          message, 'WARNING: No PROPID keyword.'
     endelse ;; no error
     CATCH, /CANCEL

     ;; Prepare for error array extension
     sxaddpar, hdr, 'EXTEND', 'T', 'SSG error array may be appended'

     ;; Put HESARC standard PARENT keyword and my additions.  Testing
     ;; is a relic from past version
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
     
     ;; Now flip so blue is to the left, slice 1 is down
     err = 0
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'there seem to be no BIASSEC, etc. keywords in this header.  Roting the image 90 degree clockwise, but skipping the header modifications',/CONTINUE
        sxaddhist, string('(ssgread.pro) Error manipulating BIASSEC, etc. keywords.'), hdr
        sxaddhist, string('(ssgread.pro) blue might not be left or slice 1 on the bottom'), hdr

     endif else begin
        if nx lt ny then begin
           ;;  Rotate all array keywords as well as the array
           im=rotate(im,1)
           sxaddhist, string('(ssgread.pro) Rotated image 90 degrees counterclockwise'), hdr
           full_array = string('[1:', nx, ',1:', ny, ']')
           asize=size(im) & nx=asize[1] & ny=asize[2]
           sxaddpar, hdr, 'NAXIS1', nx, 'size of axis 1'
           sxaddpar, hdr, 'NAXIS2', ny, 'size of axis 2'

           sxaddpar, hdr, 'BIASSEC', rotate_array_string(sxpar(hdr,'BIASSEC'), full_array, 1)
           sxaddpar, hdr, 'TRIMSEC', rotate_array_string(sxpar(hdr,'TRIMSEC'), full_array, 1)
           sxaddpar, hdr, 'DATASEC', rotate_array_string(sxpar(hdr,'DATASEC'), full_array, 1)
           sxaddpar, hdr, 'CCDSEC' , rotate_array_string(sxpar(hdr,'CCDSEC' ), full_array, 1)
           sxaddpar, hdr, 'ORIGSEC', rotate_array_string(sxpar(hdr,'ORIGSEC'), full_array, 1)

           sxaddhist, string('(ssgread.pro) so blue is left, slice 1 is bottom.'), hdr
           sxaddhist, string('(ssgread.pro) Modified *SEC keywords to reflect rotation.'), hdr
        endif else $
          sxaddhist, string('(ssgread.pro) image already has blue left, slice 1 bottom'), hdr
     endelse
     CATCH, /CANCEL

  endif ;; We have been here before

  ;; If /DATA, get non-overclock region 
  if keyword_set (data) then begin
     datasec = strtrim(sxpar(hdr,'DATASEC',COUNT=count))
     if count eq 0 then begin
        message,'WARNING: no DATASEC keyword in FITS header, using whole image', /CONTINUE
        data_im=im
        if keyword_set(eim) then $
          data_eim=eim
     endif else begin
        toks=strsplit(datasec,'[:,]')
        if N_elements(toks) ne 4 then begin
           message, 'ERROR: unknown DATASEC string format, using whole image', $
                    /CONTINUE
           data_im=im
           if keyword_set(eim) then $
             data_eim=eim
        endif else begin
           coords=strsplit(datasec,'[:,]',/extract)
           coords = coords - 1  ; Translate into IDL array reference
           data_im = im[coords[0]:coords[1],coords[2]:coords[3]]
           if keyword_set(eim) then $
             data_eim = eim[coords[0]:coords[1],coords[2]:coords[3]]
           sxaddhist, string('(ssgread.pro) extracted DATASEC region'), hdr
        endelse
     endelse
     im = data_im
     if keyword_set(eim) then $
       eim = data_eim
  endif

  ;; If /BIAS, get overclock region 
  if keyword_set (bias) then begin
     biassec = strtrim(sxpar(hdr,'BIASSEC',COUNT=count))
     if count eq 0 then begin
        message,'WARNING: no BIASSEC keyword in FITS header, using whole image', /CONTINUE
        bias_im=im
        if keyword_set(eim) then $
          bias_eim=eim
     endif else begin
        toks=strsplit(biassec,'[:,]')
        if N_elements(toks) ne 4 then begin
           message, 'ERROR: unknown BIASSEC string format, using whole image', $
                    /CONTINUE
           bias_im=im
           if keyword_set(eim) then $
             bias_eim=eim
        endif else begin
           coords=strsplit(biassec,'[:,]',/extract)
           coords = coords - 1  ; Translate into IDL array reference
           ;; The standard I am adopting is to have the whole 800
           ;; dispersion direction pixels repersented.  If there are
           ;; any ones to trim, they should be set to NAN
           bias_im = fltarr(nx, coords[3]-coords[2]+1)
           ;; There are no errors yet on a bias
           if keyword_set(eim) then $
             bias_eim = bias_im
           bias_im[*] = !values.f_nan
           bias_im[coords[0]:coords[1],*] = $
             im[coords[0]:coords[1],coords[2]:coords[3]]
           sxaddhist, string('(ssgread.pro) extracted BIASSEC region'), hdr
        endelse
     endelse
     im = bias_im
     if keyword_set(eim) then $
       eim = bias_eim
  endif

  ;; If /TRIM, set pixels outside of TRIMSEC to NAN
  if keyword_set (trim) then begin
     trimsec = strtrim(sxpar(hdr,'TRIMSEC',COUNT=count))
     if count eq 0 then begin
        message,'WARNING: no TRIMSEC keyword in FITS header, using whole image', /CONTINUE
     endif else begin
        toks=strsplit(trimsec,'[:,]')
        if N_elements(toks) ne 4 then begin
           message, 'ERROR: unknown TRIMSEC string format, using whole image', $
                    /CONTINUE
        endif else begin
           asize=size(im) & nx=asize[1] & ny=asize[2]
           coords=strsplit(trimsec,'[:,]',/extract)
           coords = coords - 1  ; Translate into IDL array reference
           mask = im
           mask[*] = !values.f_nan
           ;; Make sure we don't get an array error caused by previous trimming
           if coords[1] gt nx then coords[1] = nx - 1
           if coords[3] gt ny then coords[3] = ny - 1
           mask[coords[0]:coords[1],coords[2]:coords[3]] = 1
           im = im*mask
           if keyword_set(eim) then $
             eim = eim*mask
           sxaddhist, string('(ssgread.pro) Set pixels outside of TRIMSEC to NAN'), hdr
        endelse ;; TRIMSEC keyword OK
     endelse ;; TRIMSEC keyword present
  endif

  if keyword_set(tv) then begin
     display, im, title=filename, REUSE=reuse, zoom=zoom, rotate=rotate
  endif

  return,im
end

