;+
; $Id: ssg_db_init.pro,v 1.1 2002/10/15 14:45:23 jpmorgen Exp $

; Thu Oct 10 09:22:47 2002  jpmorgen

; ssg_db_init initializes database enties for all FITS files in a
; given directory.  With the /RECURSIVE option, processes all files in
; the whole tree.

;-
pro ssg_db_init, indir

  ;; Find all files in the directory.  Let the FITS header determine
  ;; if it is a raw file or not.
  message,'looking for FITS file in '+ indir, /CONTINUE
  files = findfile(string(indir, '/*'))
  if N_elements(files) eq 1 then begin
     if strcmp(files, '') eq 1 then $
       message, 'No files found in '+indir 
  endif

  ;; This ends up being an excercise in text editing, so make it easy
  ;; for future changes...
  ngood = 1000
  raw_dir	= 	strarr(ngood)
  raw_fname	= 	strarr(ngood)
  dir		= 	strarr(ngood)
  fname		= 	strarr(ngood)
  object	= 	strarr(ngood)
  imagetyp	= 	strarr(ngood)
  date		= 	strarr(ngood)
  time		= 	strarr(ngood)
  exptime	= 	fltarr(ngood)
  dark		=	fltarr(ngood)
  detector	= 	strarr(ngood)
  hgain		=	fltarr(ngood)
  dwell		=	intarr(ngood)
  hrdnoise	=	fltarr(ngood)
  camtemp	=	intarr(ngood)
  dewtemp	=	intarr(ngood)
  db_date	= 	strarr(ngood)
  nday_arr	=	fltarr(ngood)
  
  err=0
  ngood = 0
  for i=0,N_elements(files)-1 do begin
     ;CATCH, err
     if err ne 0 then begin
        print, !error_state.name
        message,'skipping '+ files[i]+ ' is not a recognized SSG FITS',/CONTINUE
     endif else begin
        message,'Checking '+ files[i], /CONTINUE
        im=readfits(files[i], hdr) ; This will raise an error if not FITS

        ;; DATE stuff
        ;; The database is keyed off of nday, so this is very
        ;; important to get right.

; Newer files:
; DATE-OBS= '2002-03-17T01:42:42'  /  Y2K compliant (yyyy-mm-ddThh:mm:ss)
; UT      = '01:42:42          '  /  universal time (start of
; exposure)
;
; Older files:
; DATE-OBS= '14/07/94          '  /  date (dd/mm/yy) of obs.
; UT      = '06:24:16.00       '  /  universal time
; 

        rawdate_obs = strtrim(sxpar(hdr,'DATE-OBS',COUNT=count))
        ;; Just in case HEASARC conventions of _ instead of - are
        ;; being followed
        if count eq 0 then begin
           rawdate_obs = sxpar(hdr,'DATE_OBS',COUNT=count)
           sxaddpar, hdr, 'DATE-OBS', rawdate_obs
        endif

        raw_ut = strtrim(sxpar(hdr,'UT',COUNT=count))

        datearr=strsplit(rawdate_obs,'-T:',/extract)
        if N_elements(datearr) eq 6 then begin ; New Y2K convention
           temp=strsplit(strtrim(rawdate_obs),'T',/extract)
           if NOT strcmp(temp[1], raw_ut) then $
             message, /CONTINUE, 'WARNING: DATE-OBS and UT times do not agree, using DATE-OBS version'
           juldate, float(datearr), rawjd 
        endif else begin        ; Old date format
           datearr=strsplit(rawdate_obs,'/',/extract)
           if N_elements(datearr) ne 3 then $
             message, 'WARNING: malformed DATE-OBS or DATE_OBS keyword'
           timearr=strsplit(raw_ut,':',/extract)
           if N_elements(timearr) ne 3 then $
             message, 'WARNING: malformed UT keyword'
           temp=fltarr(6)
           temp[0:2]=datearr[*]
           temp[3:5]=timearr[*]
           juldate, float(temp), rawjd
           rawdate_obs = date_conv(rawjd, 'FITS')
           ;; Even though this doesn't get written to disk, put into
           ;; header for code below
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

        ;; DEFINITION OF NDAY.  Rawjd is derived above from UT time
        ;; and date of _start_ of exposure.  Nday is going to be
        ;; related to the Julian day at the _midpoint_ of the
        ;; exposure.
        ;;
        ;; Julian date would be a fine reference, but they are a bit
        ;; large at this point (start at 1/1/4713 BC), so define our
        ;; own system, before which none of our observations were
        ;; recorded.  Someone has beat us to this idea, by making
        ;; reduced Julian day, which is the output of some handy
        ;; ASTROLIB functions.  Reduced Julian days start on
        ;; 11/16/1858 (JD=2400000), which is still a little large for
        ;; us at this point.

        ;; So, Let's define our nday=0 to be 1/1/1990 = JD 2447893

        ;; Note, julian days begin at noon.  Also, IDL julday, though
        ;; handy as a function, returns real Julian Day.  ASTROLIB's
        ;; juldate returns reduced Julian day, which is JD-2400000, or
        ;; Julian day starting from 
        nday = rawjd + (darktime/2.)/3600./24. - (julday(1,1,1990)-2400000.)

        ;; This is not the "real" place for adding NDAY to the FITS
        ;; header--I am just using it to pass info to ssg_exceptions
        sxaddpar, hdr, 'NDAY', nday, 'Decimal days of obs. midpoint since 1990-1-1T00:00:00 UT'

        ssg_exceptions, im, hdr
        
        ;; Setting the image to 0 is the flag that file should not be
        ;; added to database
        if N_elements(im) gt 1 then begin

           get_date,today
           ;; Add all files to databsse, even if nday eq 0 (can change
           ;; this later if there are too many nday = 0 cases)
           raw_dir	(ngood)	= indir
           raw_fname(ngood)= files(i)
           ;;dir	(ngood)	= 
           ;;fname	(ngood)	= 
           object	(ngood)	= sxpar(hdr, 'OBJECT')
           imagetyp	(ngood)	= sxpar(hdr, 'IMAGETYP') 
           date		(ngood)	= sxpar(hdr, 'DATE-OBS')
           time		(ngood)	= sxpar(hdr, 'UT')
           exptime	(ngood)	= sxpar(hdr, 'EXPTIME')
           dark		(ngood)	= sxpar(hdr, 'DARKTIME')
           detector	(ngood)	= sxpar(hdr, 'DETECTOR')
           hgain	(ngood)	= sxpar(hdr, 'GAIN')
           dwell	(ngood)	= sxpar(hdr, 'DWELL')
           hrdnoise	(ngood)	= sxpar(hdr, 'RDNOISE')
           camtemp	(ngood)	= sxpar(hdr, 'CAMTEMP')
           dewtemp	(ngood)	= sxpar(hdr, 'DEWTEMP')
           db_date	(ngood)	= today
           nday_arr	(ngood)	= sxpar(hdr, 'NDAY')

           ngood = ngood + 1

        endif ;; a real image
     endelse ;; CATCH if err
  endfor ;; all files in directory

  if ngood gt 0 then begin
     ;; See ssg_db_create to line up the columns properly
     oldpriv=!priv
     !priv=2

     dbopen, 'ssg_reduce',1
     dbbuild, raw_dir	[0:ngood-1], $
              raw_fname	[0:ngood-1], $
              dir	[0:ngood-1], $
              fname	[0:ngood-1], $
              object	[0:ngood-1], $
              imagetyp	[0:ngood-1], $
              date	[0:ngood-1], $
              time	[0:ngood-1], $
              exptime	[0:ngood-1], $
              dark	[0:ngood-1], $
              detector	[0:ngood-1], $
              hgain	[0:ngood-1], $
              dwell	[0:ngood-1], $
              hrdnoise	[0:ngood-1], $
              camtemp	[0:ngood-1], $
              dewtemp	[0:ngood-1], $
              db_date	[0:ngood-1], $
              nday_arr	[0:ngood-1]

     dbclose
     !priv=oldpriv

  endif ;; ngood gt 0

end
