;+
; $Id: ssg_get_nday.pro,v 1.2 2002/11/12 21:32:43 jpmorgen Exp $

; ssg_get_nday 
;
; Reads nday from FITS header, if it exists, otherwise generates one
; from FITS header values.  If REGENERATE specified or nday has not
; been recorded in hdr before, modifies DATE-OBS keyword to be Y2K
; compliant and inserts NDAY keyword into header.
;
; Calls ssg_exceptions
;
; DEFINITION OF NDAY.  Rawjd is derived above from UT time and date of
; _start_ of exposure.  Nday is going to be related to the Julian day
; at the _midpoint_ of the exposure.
;
; Julian date would be a fine reference, but they are a bit large at
; this point (start at 1/1/4713 BC), so define our own system, before
; which none of our observations were recorded.  Someone has beat us
; to this idea, by making reduced Julian day, which is the output of
; some handy ASTROLIB functions.  Reduced Julian days start on
; 11/16/1858 (JD=2400000), which is still a little large for us at
; this point.

; So, Let's define our nday=0 to be 1/1/1990 = JD 2447893

; Note, julian days begin at noon.  Also, IDL julday, though handy as
; a function, returns real Julian Day.  ASTROLIB's juldate returns
; reduced Julian day, which is JD-2400000, or Julian day starting from

; Newer files:
; DATE-OBS= '2002-03-17T01:42:42'  /  Y2K compliant (yyyy-mm-ddThh:mm:ss)
; UT      = '01:42:42          '  /  universal time (start of
; exposure)
;
; Older files:
; DATE-OBS= '14/07/94          '  /  date (dd/mm/yy) of obs.
; UT      = '06:24:16.00       '  /  universal time

;-
function ssg_get_nday, hdr, REGENERATE=regenerate, formatted=formatted_nday

  nday = 0.
  if NOT keyword_set(regenerate) then begin
     nday = sxpar(hdr, 'NDAY', count=count)
     if count gt 0 then return, nday
  endif

  sxaddhist, string('(ssg_get_nday.pro) ', systime(/UTC), ' UT'), hdr
  rawdate_obs = strtrim(sxpar(hdr,'DATE-OBS',COUNT=count))
  ;; Just in case HEASARC conventions of _ instead of - are being followed
  if count eq 0 then begin
     rawdate_obs = sxpar(hdr,'DATE_OBS',COUNT=count)
     sxaddpar, hdr, 'DATE-OBS', rawdate_obs, 'added for consistency with other SSG data'
  endif

  raw_ut = strtrim(sxpar(hdr,'UT',COUNT=count))

  datearr=strsplit(rawdate_obs,'-T:',/extract)
  if N_elements(datearr) eq 6 then begin ; New Y2K convention
     temp=strsplit(strtrim(rawdate_obs),'T',/extract)
     if NOT strcmp(temp[1], raw_ut) then $
       message, /CONTINUE, 'WARNING: DATE-OBS and UT times do not agree, using DATE-OBS version'
     juldate, float(datearr), rawjd 
  endif else begin              ; Old date format
     datearr=strsplit(rawdate_obs,'/',/extract)
     if N_elements(datearr) ne 3 then $
       message, /CONTINUE, 'WARNING: malformed DATE-OBS or DATE_OBS keyword'
     timearr=strsplit(raw_ut,':',/extract)
     if N_elements(timearr) ne 3 then $
       message, /CONTINUE, 'WARNING: malformed UT keyword'
     temp=fltarr(6)
     temp[0:2]=datearr[*]
     temp[3:5]=timearr[*]
     juldate, float(temp), rawjd
     rawdate_obs = date_conv(rawjd, 'FITS')
     sxaddpar, hdr, 'DATE-OBS', rawdate_obs
  endelse

  darktime = sxpar(hdr, 'DARKTIME',COUNT=count)
  if count eq 0 then begin
     message, /CONTINUE, 'WARNING: DARKTIME keyword not found: unlikely to be an SSG image'
     darktime = sxpar(hdr, 'EXPTIME')
     if count eq 0 then begin
        message, /CONTINUE, 'WARNING: EXPTIME keyword not found: unlikely to be an SSG image.  Using begining of the exposure for nday reference'
        darktime = 0
     endif
  endif

  nday = rawjd + (darktime/2.)/3600./24. - (julday(1,1,1990)-2400000.)
  sxaddpar, hdr, 'NDAY', nday, 'Decimal days of obs. midpoint since 1990-1-1T12:00:00 UT'

  ;; ssg_exceptions may modify nday
  ssg_exceptions, im, hdr
  nday = sxpar(hdr, 'NDAY')
  formatted = string(format='(f11.5)', nday)

  return, nday


end
