;+
; $Id: ssg_db_init.pro,v 1.9 2015/03/04 15:49:59 jpmorgen Exp $

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


  init={ssg_sysvar}
  ;; Avoid errors from db stuff about not being able to write in a
  ;; write-protected data directory.  This is more trouble that it is
  ;; worth, since if I have a database in /tmp, it gets read instead
  ;; of the one in /data/io/ssg/analysis/database
  ;;cd, '/tmp'
  silent = 1
  if keyword_set(verbose) then silent = 0
  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  ;; Find all files in the directory.
  message,'looking for FITS file in '+ indir, /CONTINUE
  files = file_search(string(indir, '/*'), /test_regular)
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
  nday_arr	=	dblarr(ngood)
  typecode	=	bytarr(ngood)
  obj_code	=	bytarr(ngood)
  bad		=	intarr(ngood)
  
  err=0
  ngood = 0
  redflag=0

  ;; Prepare to skip files that we know raise frequent errors
  skip_files = make_array(N_elements(!ssg.non_fits), value=0)

  ;; Open database for inspection only.  
  dbopen, dbname, 0
  for i=0,N_elements(files)-1 do begin
     ;;file_test(shortfile, /directory)
     shortfile= strmid(files[i], $
                       strpos(files[i], '/', /REVERSE_SEARCH) + 1)

     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + shortfile+ ' which does not appear to be a proper raw SSG FITS file',/CONTINUE
     endif else begin
        message,'Checking '+ shortfile, /INFORMATIONAL

        ;; Quietly skip known non-fits files and directories
        for inf=0,N_elements(!ssg.non_fits)-1 do begin
           skip_files[inf] = strmatch(shortfile, !ssg.non_fits[inf])
        endfor
        
        if total(skip_files) ne 0 then begin
           message, /INFORMATIONAL, 'NOTE: ' + shortfile + ' is probably not a FITS file, skipping'
           CONTINUE
        endif

        im=ssgread(files[i], hdr) ; This will raise an error if not FITS
        test = sxpar(hdr, 'BIASSEC', count=count)
        if count eq 0 then $
          message, 'NOTE: BIASSEC keyword is missing.  This is probably a post-processed file.'

        nday = sxpar(hdr, 'NDAY', count=count)
        if count gt 0 then begin
           message, 'WARNING: file ' + shortfile + ' has already been reduced.', /CONTINUE
           redflag=1
        endif

        nday = ssg_get_nday(hdr, /REGENERATE)
        formatted_nday = string(format='(f11.5)', nday)

        ssg_exceptions, im, hdr
        
        ;; Check to see if anyone was doing any unauthorized reduction
        ;; in the raw directory.  The raw FITS headers will be the
        ;; same, but the filenames will be suspicious.  The only worry
        ;; is: which comes first--the real data or the processed data?
        dup_idx = where(abs(nday_arr - nday) le !ssg.tolerance, count) 
        if count ne 0 then begin
           message, 'WARNING: duplicate nday '+ formatted_nday + ' in ' + indir + ' Checking against known post processing file names' , /CONTINUE

           ;; ADD POST PROCESSING FILE EXTENSIONS OR NAME VARIANTS HERE
           bogus_files = ['bias', 'comp', 'flat']
           
           ;; Check the current file to see if it is the postprocessed
           ;; one, in which case we just throw it out
           checkname = strlowcase(shortfile)
           for bfi = 0,N_elements(bogus_files) - 1 do begin
              if strpos(checkname, bogus_files[bfi]) ne -1 then $
                message, 'WARNING: found a post processed "' + bogus_files[bfi] + '" file'
           endfor

           ;; Now check to see if we already accepted a bogus file
           checkname = strlowcase(raw_fname(dup_idx))
           for bfi = 0,N_elements(bogus_files) - 1 do begin
              if strpos(checkname, bogus_files[bfi]) ne -1 then begin
                 temp = raw_fname(dup_idx)
                 raw_fname(dup_idx) = shortfile
                 shortfile = temp
                 message, 'WARNING: I seem to have accepted a a post processed "' + bogus_files[bfi] + '" some time back.  Swapping its name (' + shortfile + ') with what appears to be the real raw filename (' + raw_fname(dup_idx) + ').'
              endif
           endfor
           message, 'ERROR: post processing file name ' + raw_fname(dup_idx) + ' or ' + shortfile + ' not recognized.  Please add it to the variable ''bogus_files'' in this program'
        endif ;; Finding post processing files


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
        if count ge 1 then begin
           if count gt 1 then begin
              message, /CONTINUE, 'WARNING: multiple nday '+ formatted_nday + ' found in database ' + dbname + ' need to delete both to reinitialize'
           endif else $
             message, /CONTINUE, 'WARNING: entry for nday = ' + formatted_nday + ' needs to be deleted from the ' + dbname + ' database to reinitialize ' + shortfile
           if N_elements(delete_list) eq 0 then begin
              delete_list = re_init
           endif else begin
              delete_list = [delete_list, re_init]
           endelse
        endif
        temp=where(nday_arr eq nday, count) 
        if count gt 0 then $
          message, 'ERROR: duplicate nday '+ formatted_nday + ' found in this directory.  You may have to tweak things with the ssg_exceptions.pro'

        get_date,today

        raw_dir		[ngood]  = indir
        raw_fname	[ngood]  = shortfile
        ;;dir		[ngood]  = 
        ;;fname		[ngood]  = 
        object		[ngood]  = strtrim(sxpar(hdr, 'OBJECT'))
        imagetype	[ngood]  = strtrim(sxpar(hdr, 'IMAGETYP'))
        date		[ngood]  = strtrim(sxpar(hdr, 'DATE-OBS'))
        time		[ngood]  = strtrim(sxpar(hdr, 'UT'))
        exptime		[ngood]  = sxpar(hdr, 'EXPTIME')
        dark		[ngood]  = sxpar(hdr, 'DARKTIME')
        detector	[ngood]  = strtrim(sxpar(hdr, 'DETECTOR'))
        hgain		[ngood]  = sxpar(hdr, 'GAIN')
        dwell		[ngood]  = sxpar(hdr, 'DWELL')
        hrdnoise	[ngood]  = sxpar(hdr, 'RDNOISE')
        camtemp		[ngood]  = sxpar(hdr, 'CAMTEMP')
        dewtemp		[ngood]  = sxpar(hdr, 'DEWTEMP')
        db_date		[ngood]  = today
        nday_arr	[ngood]  = sxpar(hdr, 'NDAY')
        ;; Typecode takes some guessing, particularly
        ;; differentiating between lamp flats and sky flats.
        ;; Assume type is unkown and see if we end up with
        ;; something
        typecode[ngood] = 255
        if strcmp(imagetype[ngood], 'zero', 4, /fold_case) then $
          typecode[ngood] = 0
        if strcmp(imagetype[ngood], 'bias', 4, /fold_case) then $
          typecode[ngood] = 0
        if strcmp(imagetype[ngood], 'dark', 4, /fold_case) then $
          typecode[ngood] = 1
        if strcmp(imagetype[ngood], 'comp', 4, /fold_case) then $
          typecode[ngood] = 2
        if strcmp(imagetype[ngood], 'flat', 4, /fold_case) then begin
           typecode[ngood] = 3
           if (strpos(strlowcase(object[ngood]), 'sky') ne -1) then $
             typecode[ngood] = 4
        endif
        if strcmp(imagetype[ngood], 'projector flat', 4, /fold_case) then begin
           typecode[ngood] = 3
           if (strpos(strlowcase(object[ngood]), 'sky') ne -1) then $
             typecode[ngood] = 4
        endif
        if strcmp(imagetype[ngood], 'object', 6, /fold_case) then begin
           typecode[ngood] = 5
           if (strpos(strlowcase(object[ngood]), 'sky flat') ne -1) then $
             typecode[ngood] = 4
        endif
        if typecode[ngood] eq 255 then $
          message, 'WARNING: unknown IMAGETYP keyword ' + imagetype[ngood] + ' Please make a typecode for this', /CONTINUE

        
        ;; obj_code.  Assume type is unknown and see if we end up with
        ;; something
        obj_code[ngood] = 255

        ;; Try to do a little bit of intelligent parsing of the object
        ;; string to figure out what object we are looking at when we
        ;; mention more than one object.  For instance,  2002-03-16
        ;; OBJECT "Io-West Series (Europa safely past)".  The right
        ;; answer tends to be the object named first
        obj_regexps = ['jup','io', 'eur', 'gan', 'call']
        N_regexp = N_elements(obj_regexps)
        ;; Store the positions of the object names in the OBJECT
        ;; string in an array
        obj_strposes = make_array(N_regexp, value=-1)
        for ire=0, N_regexp-1 do begin
           obj_strposes[ire] = strpos(strlowcase(object[ngood]), $
                                      obj_regexps[ire])
        endfor ;; each regexp
        ;; stropos returns -1 when a string is not found.  Set those
        ;; to the highest value
        bad_idx = where(obj_strposes eq -1, count)
        if count gt 0 then $
           obj_strposes[bad_idx] = max(obj_strposes) + 1
        ;; Find the minimum subscript (usually 0), which will end up
        ;; being the obj_code we want.  NOTE: we have to extract the
        ;; subscript with a variable, since obj_code[ngood] is passed
        ;; by value, not reference
        junk = min(obj_strposes, obj_regexp_idx)
        obj_code[ngood] = obj_regexp_idx

        ;; The above code replaces this, which got confused when
        ;; multiple objects were mentioned
        ;; if strpos(strlowcase(object[ngood]), 'jup') ne -1 then $
        ;;   obj_code[ngood] = 0
        ;; if strpos(strlowcase(object[ngood]), 'io')  ne -1 then $
        ;;   obj_code[ngood] = 1
        ;; if strpos(strlowcase(object[ngood]), 'eur') ne -1  then $
        ;;   obj_code[ngood] = 2
        ;; if strpos(strlowcase(object[ngood]), 'gan') ne -1 then $
        ;;   obj_code[ngood] = 3
        ;; if strpos(strlowcase(object[ngood]), 'call') ne -1 then $
        ;;    obj_code[ngood] = 4

        ;; Night sky
        if typecode[ngood] ne 4 then begin ;; Not a sky flat
           if (strpos(strlowcase(object[ngood]), 'sky') ne -1) then $
             obj_code[ngood] = 5
        endif

        ;; I don't really need to assign object codes to these, but
        ;; might as well for completeness
        if typecode[ngood] eq 0 then $
          obj_code[ngood] = 6
        if typecode[ngood] gt 0 and typecode[ngood] le 4 then $
          obj_code[ngood] = typecode[ngood] + 5
        if typecode[ngood] eq 2 then $ ; comp
          obj_code[ngood] = 7
        if obj_code[ngood] eq 255 then $
          message, 'WARNING: unknown OBJECT keyword ' + object[ngood] + ' Please make an obj_code for this or fix the header in ssg_exceptions.  NDAY =  ' + string(nday_arr[ngood]), /CONTINUE

        bad		[ngood] = sxpar(hdr, 'SSG_BAD')
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
  if N_elements(delete_list) gt 0 then begin
     if !priv ge 3 then begin
        message, /CONTINUE, 'WARNING, !priv set to ' + string(!priv) + ', continuing automatically with database deletes'
     endif 
     if keyword_set(delete) then !priv = 3
     if !priv lt 3 then message, 'ERROR: deletions from the database need to be made, so !priv needs to be at least 3.  Alternately (and preferred), you can specify the /DELETE option to this procedure.  Hint, you will probably want /APPEND too'
     dbdelete, delete_list, dbname
     ;; Thu Jan 27 13:42:24 2011  jpmorgen
     ;;message, /INFORMATIONAL, 'deleted items marked'
     ;; Latest version of dbdelete does not need dbcompress
     ;;dbcompress, dbname
     ;; Quietly change the mode and group of the file to be group lyra
     ;; writable so other people can work on this
     dbfname = find_with_def(dbname + '.dbf','ZDBASE')
     spawn, string('chmod g+w ', dbfname), txtout, errout
     spawn, string('chgrp lyra ', dbfname), txtout, errout
     message, /INFORMATIONAL, 'items deleted removed, database group lyra writable'
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
              obj_code	[0:ngood-1], $
              bad	[0:ngood-1]

     dbclose
     message, /INFORMATIONAL, 'database appended and closed'
     
  endif ;; ngood gt 0

  !priv=oldpriv

end
