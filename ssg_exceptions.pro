;+
; $Id: ssg_exceptions.pro,v 1.2 2003/06/11 18:12:51 jpmorgen Exp jpmorgen $

; ssg_exceptions Make unusual modifications to the image, FITS header,
; or database that would otherwise be too difficult to program in to
; the normal processing software

; Flag for ssg_db_init: set im=0

;-
pro ssg_exceptions, in_im, in_hdr

  ON_ERROR, 2

  im = in_im
  hdr = in_hdr
  test = sxpar(hdr, 'PARENTH', COUNT=count)
  if count eq 0 then begin
     message, 'ERROR: file must have been read with ssgread'
  endif
  nday = sxpar(hdr, 'NDAY', COUNT=count)
  if count eq 0 then begin
     message, 'ERROR: nday needs to be set first.  In other words, run ssg_db_init first'
  endif
  nday = ssg_get_nday(hdr, formatted=formatted_nday)
  if nday le 0 then begin
     message, 'ERRROR: nday = '+ formatted_nday + ' Use ssg_fix_head so correct this problem'
  endif
  
  ;; Now we know that the image is in the proper orientation and has a
  ;; good nday

  ;; Put any hand-derived code here

  sxaddhist, string('(ssg_exceptions.pro) ', systime(/UTC), ' UT'), hdr
;   ;; Both TI4 and TI5
;   if nday gt 4000 then begin
;      ;; Check to make sure that the TRIMSEC and BIASSEC keywords are
;      ;; uniform in the CCD clocking direction (which is now the X coordinate)
;      
;      trimsec = strtrim(sxpar(hdr,'TRIMSEC',COUNT=count))
;      coords=strsplit(trimsec,'[,]',/extract)
;      if coords[0] ne '1:799' then begin
;         message, /CONTINUE, 'NOTE: TRIMSEC was ' + trimsec + ' Changing first part to 1:799]'
;         modified = 1
;         sxaddpar, hdr, 'TRIMSEC', string('[1:799,', coords[1], ']')
;         sxaddhist, string('(ssg_exceptions.pro) Made uniform TRIMSEC keyword for TI5'), hdr
;      endif
;      biassec = strtrim(sxpar(hdr,'BIASSEC',COUNT=count))
;      coords=strsplit(biassec,'[,]',/extract)
;      if coords[0] ne '1:799' then begin
;         message, /CONTINUE, 'NOTE: BIASSEC was ' + biassec + ' Changing first part to 1:799]'
;         modified = 1
;         sxaddpar, hdr, 'BIASSEC', string('[1:799,', coords[1], ']')
;         sxaddhist, string('(ssg_exceptions.pro) Made uniform BIASSEC keyword for TI5'), hdr
;      endif
;   endif ;; TI5
  ;; BEGIN AUTOMATIC SECTION
  ;; END AUTOMATIC SECTION

  ;; Fix a spelling error
  object = strtrim(sxpar(hdr, 'OBJECT'))
  if object eq 'To west series' then begin
     modified = 1
     sxaddpar, hdr, 'OOBJECT', object, after='OBJECT'
     object = 'Io west series'
     sxaddpar, hdr, 'OBJECT', object
  endif

  ;; Ron and Ed changed the number of columns on 2007-06-04
  ;; --> still working on this
  if fix(nday) eq 6363 then begin
     dims = size(im, /dimensions)
     if dims[0] ne 189 then begin
        modified = 1
        im = im[dims[0]-189:dims[0]-1,*]
        ccdsec = sxpar(hdr, 'CCDSEC')
        sxaddpar, hdr, 'OCCDSEC', ccdsec, after='CCDSEC'
        datasec = sxpar(hdr, 'DATASEC')
        sxaddpar, hdr, 'ODATASE', ccdsec, after='DATASEC'
        ccdsec = sxpar(hdr, 'TRIMSEC')
        sxaddpar, hdr, 'OTRIMSE', ccdsec, after='TRIMSEC'
     endif ;; One of the extra-wide flats
  endif ;; 2007-06-04


  if keyword_set(modified) then begin
     in_im = im
     in_hdr = hdr
  endif ;; modified

end
