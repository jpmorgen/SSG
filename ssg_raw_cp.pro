;+
; $Id: ssg_raw_cp.pro,v 1.2 2002/10/28 19:57:44 jpmorgen Exp $

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
  writefits, string(outdir, '/test_ssg_raw_cp_writable'), [0]

  ;; Cut off trailing / from outdir so it looks pretty in database
  if strmid(outdir, 0, 1, /REVERSE_OFFSET) eq '/' then $
    outdir = strmid(outdir, 0, strlen(outdir)-1)

  dbclose ;; Just in case
  ;; Find all files in the directory.  
  dbname = 'ssg_reduce'
  dbopen, dbname, 0

  files = findfile(string(indir, '/*')) ; Doesn't matter if <dir>//*
  if N_elements(files) eq 1 then begin
     if strcmp(files, '') eq 1 then $
       message, 'No files found in '+indir 
  endif

  nf = N_elements(files)

  entries = intarr(nf)
  entries[*] = -1               ; Flag for bad file
  ngood = 0

  err=0

  for i=0,N_elements(files)-1 do begin
     shortfile= strmid(files[i], $
                       strpos(files[i], '/', /REVERSE_SEARCH) + 1)

     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + shortfile, /CONTINUE
     endif else begin
        message,'Checking file '+ shortfile, /INFORMATIONAL
        im=readfits(files[i], hdr, SILENT=silent) ; This will raise an error if not FITS

        nday = ssg_get_nday(hdr)
        formatted_nday = string(format='(f11.5)', nday)

        entries[i] = where_nday_eq(nday, count=count, SILENT=silent)
        ;; These errors are caught by the code above and the files are skipped
        if count eq 0 then message, 'WARNING: no entry for ' + shortfile + ' nday = '+ formatted_nday + ' found in ' + dbname + '.  Did you run ssg_db_init ' + indir
        if count gt 1 then message, 'ERROR: Duplicate nday = ' + formatted_nday + ' in ' + dbname + ' database.  This should never happen'

        sxaddhist, string('(ssg_raw_cp.pro) ', systime(/UTC), ' UT'), hdr
        sxaddpar, hdr, 'RAWFILE', files[i], 'Full path to raw SSG file'
        sxaddpar, hdr, 'PARENT', files[i], 'Parent file'
        sxaddpar, hdr, 'SSGFILE', shortfile, 'Reduced filename'
        dbext, entries[i], 'db_date, typecode', db_date, typecode
        sxaddpar, hdr, 'DB_DATE', db_date[0], 'FIRST database entry date (yyyy-mm-dd UT)'
        sxaddpar, hdr, 'TYPECODE', uint(typecode[0]), '0=bias,1=dark,2=comp,3=flat,4=sky flat,5=object'

        ;; rotate image to proper orientation
        im = ssgread(im, hdr)

        ;; In case there is a name change or the like.
        ssg_exceptions, im, hdr

        shortfile = sxpar(hdr, 'SSGFILE')
        files[i] = shortfile

        outfile = string(outdir, '/', shortfile)
        checkout = findfile(outfile, COUNT=count)
        if count ne 0 and NOT keyword_set(overwrite) then begin
           message, 'WARNING: file ' + shortfile + ' found in ' + outdir + ' use /OVERWRITE keyword to replace'
        endif
        
        message, /INFORMATIONAL, 'Writing ' + outfile
        ;; Code from writefits so that I have control over verbosity
        ;; (no silent keyword for writefits)
        check_fits, im, hdr, /UPDATE, /FITS, SILENT=silent
        writefits, outfile, im, hdr
        ngood=ngood+1

     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  dbclose

  good_files = strarr(ngood)
  dbdir = strarr(ngood)
  dbdir[*] = strarr(ngood)
  good_entries = intarr(ngood)
  good_entries[*] = entries[where(entries ne -1)]

  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, good_entries, 'dir, fname', dbdir, good_files
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated' + string(ngood) + ' filenames in ' + dbname
  ;; For convenience 
  cd, outdir
  message, /INFORMATIONAL, 'Directory is set to ' + outdir
end
