;+
; $Id: ssg_db_init.pro,v 1.3 2002/11/12 20:57:07 jpmorgen Exp $

; ssg_db_init initializes database enties for all FITS files in a
; given directory.  The idea is to have each file entered once and
; only once into the database.  To do this, the nday of each file is
; calculated (nday = number of elapsed Julian days from 1/1/1990 to
; the midpoint of the exposure) and compared to the ndays in the rest
; of the database.  If a duplicate is found, it is assumed you are
; refreshing that entry.  The most practical way to refresh an entry
; is to delete it and re-append it.  But never fear, nothing will be
; deleted unless you specify an option (/DELETE) or have !priv set to
; 3 or more.  Similarly, nothing will be appended unless you have
; /APPEND set or !priv set to 2 or more.  If you find this latter
; feature a pain, change it to NOAPPEND, but keep the logic of
; separate DELETE and APPEND functions, since this lets you delete
; selected files from the database (see ssg_nday_delete)

; TO DO: With the /RECURSIVE option, processes all files in the whole
; tree.

;-
pro ssg_db_init, indir, APPEND=append, DELETE=delete, NONRAW=nonraw, VERBOSE=verbose


  silent = 1
  if keyword_set(verbose) then silent = 0
  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  ;; Find all files in the directory.
  message,'looking for FITS file in '+ indir, /CONTINUE
  files = findfile(string(indir, '/*'))
  if N_elements(files) eq 1 then begin
     if strcmp(files, '') eq 1 then $
       message, 'No files found in '+indir 
  endif


  ;; This ends up being an excercise in text editing, so make it easy
  ;; for future changes...
  ngood = 1000
  delete_list	=	intarr(ngood)
  raw_dir	= 	strarr(ngood)
  raw_fname	= 	strarr(ngood)
  dir		= 	strarr(ngood)
  fname		= 	strarr(ngood)
  object	= 	strarr(ngood)
  imagetype	= 	strarr(ngood)
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
  typecode	=	bytarr(ngood)
  bad		=	intarr(ngood)
  
  err=0
  ngood = 0
  ndelete = 0
  redflag=0
  ;; Open database for inspection only.  
  dbopen, dbname, 0
  for i=0,N_elements(files)-1 do begin
     shortfile= strmid(files[i], $
                       strpos(files[i], '/', /REVERSE_SEARCH) + 1)

     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, shortfile+ ' is not a recognized SSG FITS file',/CONTINUE
     endif else begin
        message,'Checking '+ shortfile, /INFORMATIONAL
        im=readfits(files[i], hdr, SILENT=silent) ; This will raise an error if not FITS
        nday = sxpar(hdr, 'NDAY', count=count)
        if count gt 0 then begin
           message, 'WARNING: file ' + shortfile + ' has already been reduced.', /CONTINUE
           redflag=1
        endif

        nday = ssg_get_nday(hdr, /REGENERATE)
        formatted_nday = string(format='(f11.5)', nday)

        ssg_exceptions, im, hdr

        ;; Setting the image to 0 (or some other scaler) is the flag
        ;; that file should not be added to database
        if N_elements(im) le 1 then $
          message, 'WARNING: no image found for ' + shortfile
        if nday le 0 then $
          message, 'WARNING: nday = ' + formatted_nday + ' for ' + shortfile

        ;; We have a genuine image to add to or reset in the database.
        ;; Also check for duplicates in database and in other files we
        ;; are adding this time around.

        message, shortfile + ' seems to be worth adding to ' + dbname, /INFORMATIONAL
        re_init = where_nday_eq(nday, COUNT=count,SILENT=silent)
        if count eq 1 then begin
           message, /CONTINUE, 'WARNING: entry for nday = ' + formatted_nday + ' needs to be deleted from the ' + dbname + ' database to reinitialize ' + shortfile
           delete_list(ndelete) = re_init
           ndelete = ndelete + 1
        endif 
        if count gt 1 then $
          message, 'ERROR: duplicate nday '+ formatted_nday + ' found in database' + dbname + ' this should never happen'
        temp=where(nday_arr eq nday, count) 
        if count gt 0 then $
          message, 'ERROR: duplicate nday '+ formatted_nday + ' found in this directory.  You may have to tweak things with the ssg_exceptions.pro'

        get_date,today

        raw_dir		(ngood)	= indir
        raw_fname	(ngood) = shortfile
        ;;dir		(ngood)	= 
        ;;fname		(ngood)	= 
        object		(ngood)	= strtrim(sxpar(hdr, 'OBJECT'))
        imagetype	(ngood)	= strtrim(sxpar(hdr, 'IMAGETYP'))
        date		(ngood)	= strtrim(sxpar(hdr, 'DATE-OBS'))
        time		(ngood)	= strtrim(sxpar(hdr, 'UT'))
        exptime		(ngood)	= sxpar(hdr, 'EXPTIME')
        dark		(ngood)	= sxpar(hdr, 'DARKTIME')
        detector	(ngood)	= strtrim(sxpar(hdr, 'DETECTOR'))
        hgain		(ngood)	= sxpar(hdr, 'GAIN')
        dwell		(ngood)	= sxpar(hdr, 'DWELL')
        hrdnoise	(ngood)	= sxpar(hdr, 'RDNOISE')
        camtemp		(ngood)	= sxpar(hdr, 'CAMTEMP')
        dewtemp		(ngood)	= sxpar(hdr, 'DEWTEMP')
        db_date		(ngood)	= today
        nday_arr	(ngood)	= sxpar(hdr, 'NDAY')
        ;; Typecode takes some guessing, particularly
        ;; differentiating between lamp flats and sky flats.
        ;; Assume type is unkown and see if we end up with
        ;; something
        typecode(ngood) = 255
        if strcmp(imagetype(ngood), 'zero', 4, /fold_case) then $
          typecode(ngood) = 0
        if strcmp(imagetype(ngood), 'dark', 4, /fold_case) then $
          typecode(ngood) = 1
        if strcmp(imagetype(ngood), 'comp', 4, /fold_case) then $
          typecode(ngood) = 2
        if strcmp(imagetype(ngood), 'flat', 4, /fold_case) then begin
           typecode(ngood) = 3
           if (strpos(strlowcase(object(ngood)), 'sky') ne -1) then $
             typecode(ngood) = 4
        endif
        if strcmp(imagetype(ngood), 'object', 6, /fold_case) then begin
           typecode(ngood) = 5
           if (strpos(strlowcase(object(ngood)), 'sky flat') ne -1) then $
             typecode(ngood) = 4
        endif
        if typecode(ngood) eq 255 then $
          message, 'WARNING: unknown IMAGETYP keyword ' + imagetype(ngood) + ' Please make a typecode for this', /CONTINUE
        bad		(ngood) = sxpar(hdr, 'SSG_BAD')
        ngood = ngood + 1

     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  dbclose

  oldpriv=!priv

  ON_ERROR, 2

  if redflag ne 0 and NOT keyword_set(nonraw) then begin 
     message, 'ERROR: Reinitializing the database using reduced files can lead to unexpected results.  I recommend you always start from raw files and work foward from these.  If you know what you are doing, go ahead and specify the /NONRAW flag and I''ll look the other way this time.'
  endif

  ;; DO DATABASE DELETES
  if ndelete gt 0 then begin
     if !priv ge 3 then begin
        message, /CONTINUE, 'WARNING, !priv set to ' + string(!priv) + ', continuing automatically with database deletes'
     endif 
     if keyword_set(delete) then !priv = 3
     if !priv lt 3 then message, 'ERROR: deletions from the database need to be made, so !priv needs to be at least 3.  Alternately (and preferred), you can specify the /DELETE option to this procedure.  Hint, you will probably want /APPEND too'
     dbdelete, delete_list[0:ndelete-1], dbname
     message, /INFORMATIONAL, 'deleted items marked'
     dbcompress, dbname
     message, /INFORMATIONAL, 'deleted items removed'
     dbclose
     message, /INFORMATIONAL, 'database closed'
  endif
  !priv = oldpriv

  ;; DO DATABASE APPENDS
  if ngood gt 0 then begin
     ;; See ssg_db_create to line up the columns properly
     message, /INFORMATIONAL, 'Found ' + string(ngood) + ' files to add to database'

     if !priv ge 2 then begin
        message, /CONTINUE, 'WARNING, !priv set to ' + string(!priv) + ', continuing automatically with database appends'
     endif 
     if keyword_set(append) then !priv = 2
     if !priv lt 2 then message, 'ERROR: appends to the database need to be made, so !priv needs to be at least 2.  Alternately (and preferred), you can specify the /APPEND option to this procedure.'

     message, /INFORMATIONAL, 'Appending items to the '+ dbname + ' database'
     dbopen, dbname, 1
     dbbuild, raw_dir	[0:ngood-1], $
              raw_fname	[0:ngood-1], $
              dir	[0:ngood-1], $
              fname	[0:ngood-1], $
              object	[0:ngood-1], $
              imagetype	[0:ngood-1], $
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
              nday_arr	[0:ngood-1], $
              typecode	[0:ngood-1], $
              bad	[0:ngood-1]

     dbclose
     message, /INFORMATIONAL, 'database appended and closed'
     
  endif ;; ngood gt 0

  !priv=oldpriv

end
