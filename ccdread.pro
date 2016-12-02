; $Id: ccdread.pro,v 1.3 2015/03/04 15:44:21 jpmorgen Exp $

; ccdread this is basically just an alias for readfits and display
; One important point is that the array is returned as a float unless
; /RAW or /DOUBLE are specified


function ccdread, filename, hdr, RAW=raw, DOUBLE=double, TV=tv, REUSE=reuse, zoom=zoom, rotate=rotate, SILENT=silent

CATCH, err
if err ne 0 then begin
   CATCH, /CANCEL
   message, 'ERROR: returning -1', /CONTINUE
   return, -1
endif

im=readfits(filename, hdr, SILENT=silent)

if keyword_set(tv) then begin
    display, im, title=filename, REUSE=reuse, zoom=zoom, rotate=rotate
endif

if keyword_set(raw) then return, im

;; If we made it here, we are going to tweak the type of the image to
;; float or double.  In 2006, Wayne Landsman changed the behavior of
;; readfits so that BZERO and BSCALE are no longer tweaked to 0 and 1
;; for unsigned integer types.  This means I need to do it by hand
;; here.

Bscale = sxpar( hdr, 'BSCALE' , Count = N_bscale)
Bzero = sxpar(hdr, 'BZERO', Count = N_Bzero )

if N_Bscale GT 0  then begin
   if ( Bscale NE 1. ) then begin
      sxaddpar, hdr, 'BSCALE', 1.
      sxaddpar, hdr, 'O_BSCALE', Bscale,' Original BSCALE Value', AFTER='BSCALE'
   endif 
endif

if N_Bzero GT 0  then begin
   if (Bzero NE 0) then begin
      sxaddpar, hdr, 'BZERO', 0.
      sxaddpar, hdr, 'O_BZERO', Bzero,' Original BZERO Value', AFTER='BZERO'
   endif
endif

;; Just to be correct, we should change the BITPIX keyword as well,
;; though that tends to get set correctly when we write

if keyword_set(double) then begin
   sxaddpar, hdr, 'BITPIX', -64, 'IEEE double precision floating point '
   return, double(im)
endif

sxaddpar, hdr, 'BITPIX', -32, ' IEEE single precision floating point '
return, float(im)

end

