; $Id: ccdread.pro,v 1.2 2002/12/16 20:04:12 jpmorgen Exp $

; ccd_read this is basically just an alias for readfits and display
; One important point is that the array is returned as a float unless
; /RAW or /DOUBLE are specified


function ccdread, filename, hdr, RAW=raw, DOUBLE=double, TV=tv, REUSE=reuse, zoom=zoom, rotate=rotate, SILENT=silent

CATCH, err
if err ne 0 then begin
   message, 'ERROR: returning -1', /CONTINUE
   return, -1
endif

im=readfits(filename, hdr, SILENT=silent)

if keyword_set(tv) then begin
    display, im, title=filename, REUSE=reuse, zoom=zoom, rotate=rotate
endif

if keyword_set(raw) then return, im
if keyword_set(double) then return, double(im)
return, float(im)

end

