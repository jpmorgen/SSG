;+
; $Id: ssg_raw_cp.pro,v 1.6 2003/06/11 18:08:19 jpmorgen Exp jpmorgen $

; ssg_raw_cp.  Copies ssg FITS files from indir to outdir.  Only
; copies files that are registered as good files in the database (see
; ssg_db_init).  Rotates the images to a common orientation (red
; right, slice 1 bottom) and standardizes header values (e.g. Y2K
; dates)


;-

pro ssg_raw_cp, indir, outdir, OVERWRITE=overwrite, VERBOSE=verbose

  ON_ERROR, 2
  silent=1
  if keyword_set(verbose) then silent = 0
  cd, indir
  if NOT keyword_set(outdir) then message, 'ERROR: outdir must be supplied'
  writefits, outdir + '/test_ssg_raw_cp_writable', [0]
  spawn, string('rm ', outdir, '/test_ssg_raw_cp_writable')

  ;; Cut off trailing / from outdir so it looks pretty in database
  if strmid(outdir, 0, 1, /REVERSE_OFFSET) eq '/' then $
    outdir = strmid(outdir, 0, strlen(outdir)-1)

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0

  ;; Find all files in the directory, matching ndays and pulling over
  ;; the correct file name
  files = findfile(string(indir, '/*')) ; Doesn't matter if <dir>//*
  files = strtrim(files)
  if N_elements(files) eq 1 then begin
     if strcmp(files, '') eq 1 then $
       message, 'No files found in '+indir 
  endif

  nf = N_elements(files)

  entries = intarr(nf)
  entries[*] = -1               ; Flag for bad file
  ngood = 0

  err=0

  for i=0,nf-1 do begin
     shortinfile= strmid(files[i], $
                       strpos(files[i], '/', /REVERSE_SEARCH) + 1)
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + shortinfile, /CONTINUE
     endif else begin
        message,'Checking file '+ shortinfile, /INFORMATIONAL
        
        ;; We have to get the nday from the file so we aren't confused
        ;; by multiple filenames.  This will raise an error if not
        ;; FITS, which is fine, since entries[i] will be left=-1
        im=ssgread(files[i], hdr) 
        asize=size(im) & nx=asize[1] & ny=asize[2]

        nday = ssg_get_nday(hdr, formatted=formatted_nday)

        db_entry = where_nday_eq(nday, count=count, SILENT=silent)
        ;; These messages raise errors which are caught by the code
        ;; above and the files are skipped
        if count eq 0 then message, 'WARNING: no entry for ' + shortinfile + ' nday = '+ formatted_nday + ' found in ' + dbname + '.  Did you run ssg_db_init ' + indir + '?'
        if count gt 1 then message, 'ERROR: Duplicate nday = ' + formatted_nday + ' in ' + dbname + ' database.  This should never happen'
 
        ;; In case there is a name change or the like.
        ssg_exceptions, im, hdr
        
        dbext, db_entry, 'raw_fname, db_date, typecode', raw_fname, db_date, typecode
        
        raw_fname = strtrim(raw_fname)
        ;; Make sure we don't copy any bogus files
        if strcmp(raw_fname[0], shortinfile, /FOLD_CASE) eq 0 then begin
           message, 'WARNING: file ' + shortinfile + ' has the same nday as ' + raw_fname[0] + ' suggesting it is an old post-processed file'
        endif

        ;; If we made it here, we have a valid file that was
        ;; previously entered into the database

        ;; Ron wants the output files to have a different name from
        ;; the input files.  Here is where that modification is made
        if strmatch(shortinfile, '*.fits*') then begin
           ;; Our normal file name
           temp=strsplit(shortinfile, '.fits', /extract, /regex)
           shortoutfile = temp[0]+'r.fits'
        endif else begin
           if strmatch(shortinfile, '*.fts*') then begin
              ;; Variant
              temp=strsplit(shortinfile, '.fts', /extract, /regex)
              shortoutfile = temp[0]+'r.fits'
           endif else begin
              ;; Just in case there is some real file with a funny name.
              ;; If it is not a real SSG file, that will be caught below
              message, 'WARNING: non-standard SSG filename, appending an "r" to the name to differentiate original and copy we are doing reductions on', /CONTINUE
              shortoutfile = shortinfile+'r'
           endelse
        endelse

        entries[i] = db_entry

        sxaddhist, string('(ssg_raw_cp.pro) ', systime(/UTC), ' UT'), hdr
        sxaddhist, string('(ssg_raw_cp.pro) Read in RAWFILE, got proper NDAY, will write SSGFILE'), hdr
        sxaddpar, hdr, 'RAWFILE', files[i], 'Full path to raw SSG file'
        sxaddpar, hdr, 'PARENT', files[i], 'Parent file'
        sxaddpar, hdr, 'SSGFILE', shortoutfile, 'Reduced filename'

        sxaddpar, hdr, 'DB_DATE', db_date[0], 'FIRST database entry date (yyyy-mm-dd UT)'
        sxaddpar, hdr, 'TYPECODE', fix(typecode[0]), '0=bias,1=dark,2=comp,3=flat,4=sky flat,5=object'

        files[i] = shortoutfile

        outfile = outdir + '/' + shortoutfile
        checkout = findfile(outfile, COUNT=count)
        if count ne 0 and NOT keyword_set(overwrite) then begin
           message, 'WARNING: file ' + shortoutfile + ' found in ' + outdir + ' use /OVERWRITE keyword to replace'
        endif
        
        message, /INFORMATIONAL, 'Creating uncertainty extension and writing ' + outfile

        ;;  Make a simple FITS header for the extension
        sxaddpar, hdr, 'EXTEND', 'T', 'SSG error array is appended'
        sxaddpar, ehdr, 'XTENSION', 'IMAGE', 'IMAGE extension'
        sxaddpar, ehdr, 'NAXIS', sxpar(hdr, 'NAXIS'), 'Number of data axes'
        sxaddpar, ehdr, 'NAXIS1', nx
        sxaddpar, ehdr, 'NAXIS2', ny
        sxaddpar, ehdr, 'EXTNAME', 'ERROR', 'Statistical errors of primary array'
        eim = fltarr(nx,ny)
        ssgwrite, outfile, im, hdr, eim, ehdr
        ngood=ngood+1

     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  print, db_info( 'ENTRIES' )
  print, db_info( 'NUMBER' )
  dbclose

  if ngood eq 0 then message, 'ERROR: no files copied, database not updated'
  good_files = strarr(ngood)
  dbdir = strarr(ngood)
  dbdir[*] = outdir
  good_entries = intarr(ngood)
  good_idx = where(entries ne -1)
  good_entries[*] = entries[good_idx]
  good_files[*] = files[good_idx]
;;
;;print, dbdir
;;print, entries
;;print, files
;;
;;print, good_entries
;;print, good_files

;;help, good_entries, dbdir, good_files
;;
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, good_entries, 'dir, fname', dbdir, good_files
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated' + string(ngood) + ' filenames in ' + dbname
  ;; For convenience 
  ;;cd, outdir
  message, /INFORMATIONAL, 'Directory is set to ' + outdir

  dbopen, dbname, 0
  print, db_info( 'ENTRIES' )
  print, db_info( 'NUMBER' )
  dbclose

end
