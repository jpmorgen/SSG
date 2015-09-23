;+
; $Id: ssg_fix_head.pro,v 1.7 2015/03/04 15:50:21 jpmorgen Exp $

; ssg_fix_head.  Fix up Y2K and other header problems with the FITS
; headers ON THE RAW FILES.  Saves a copy of all the affected files in
; case there is some catastrophic error in this process.

;-

pro ssg_fix_head, indir, outdir
  
;  ON_ERROR, 2
  cd, indir

  if NOT keyword_set(outdir) then $
    outdir = indir
  
  ;; Check to see if we can write in this directory
  CATCH, err
  if err ne 0 then begin
     message, /NONAME, !error_state.msg, /CONTINUE
     message, /CONTINUE, 'WARNING: output directory ' + outdir + ' is not writable by you.  Using /tmp instead '
     outdir = '/tmp'
  endif else begin
     testname = string(outdir, '/test_ssg_fix_head_writable')
     writefits, testname, [0]
  endelse
  CATCH, /CANCEL

  ;; Clean up
  cd, outdir
  spawn, 'rm -f ' + testname
  cd, indir

  ;; See if there are any files listed in the directory
  files = file_search(string(indir, '/*'), /test_regular) ; Doesn't matter if <dir>//*
  files = strtrim(files)
  if N_elements(files) eq 1 then begin
     if strcmp(files, '') eq 1 then $
       message, 'No files found in '+indir 
  endif

  ;; Get ready to create a tar file to which the original files are
  ;; written
  outname = indir
  outarr=strsplit(outname,'/', /extract)
  outname = 'ORIG_'
  for i=0,N_elements(outarr) - 1 do begin
     outname = outname + '_' + outarr[i]
  endfor
  outname = outdir + '/' + outname + '.tar'
  test = file_search(outname+'.gz', count=count)
  if count eq 1 then begin
     message, 'WARNING: Previous run of ssg_fix_head detected.  If you really want to run this again, move '+ outname+ '.gz to a safe place.', /CONTINUE
     message, 'WARNING: EXITING WITHOUT CHECKING HEADERS!', /CONTINUE
     return
  endif
  test = file_search(outname, count=count)
  if count eq 1 then $
    message, 'ERROR: Program did not terminate properly.  I suggest you start over with a tar xvf '+ outname + ' and move that file to a different name'

  ;; Now fix the headers.  Only bad files are rewritten
  nf = N_elements(files)
  num_bad = 0
  for i=0,nf-1 do begin
     bad_file = 0
     shortinfile = strmid(files[i], $
                          strpos(files[i], '/', /REVERSE_SEARCH) + 1)
     outfile = shortinfile
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + shortinfile, /CONTINUE
     endif else begin
        message,'Checking file '+ shortinfile, /INFORMATIONAL
        if strpos(shortinfile, 'ORIG_') ne -1 or $
          strpos(shortinfile, '.tar') ne -1 or $
          strpos(shortinfile, '.tgz') ne -1 then $
          message, 'Oops, hit a tar archive'
        
        ;; Any errors generated here bump us to the next file in the loop
        im=readfits(files[i], hdr)
        sxaddhist, string('(ssg_fix_head.pro) ', systime(/UTC), ' UT'), hdr

        ;; Make sure this is really an SSG file (see also code in ssgread)
        origin = sxpar(hdr, 'ORIGIN')
        if strtrim(origin) ne 'KPNO-IRAF' then $
          message, 'ORIGIN keyword is ' + string(origin) + ' not KPNO-IRAF'
        test = sxpar(hdr, 'IRAFNAME', count=count)
        if count eq 0 then $
          message, 'No IRAFNAME keyword.'

        ;; Really old files don't have these, so put them in
        observat = sxpar(hdr, 'OBSERVAT', count=count)
        if count eq 0 then begin
           observat = 'NSO'
           sxaddpar, hdr, observat, 'observatory'
        endif
        if strtrim(observat) ne 'NSO' then $
          message, 'OBSERVAT keyword is ' + string(observat) + ' not NSO'

        months = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']
        ;; OK, now we get down to the serious business.  I have found
        ;; two Y2K-related bugs in the FITS header system.  One has
        ;; the year off by one (Claude & Co forgot to change the
        ;; hard-wired date), and the other is that DATE-OBS and other
        ;; useful things, like the telescope position, are missing.
        ;; In the later case, UT is there, so at least some of the
        ;; other quantities can be calculated from the DATE and/or
        ;; filename.  For old files, sometimes the DATE-OBS keyword is
        ;; wrong.

        ut = sxpar(hdr, 'UT', count=count)
        if count eq 0 then $
          message, 'No UT keyword.'

        ;; Get dates from the filename and directory
        dir_array = strsplit(indir, '/', /extract)
        
        ;; YEAR.  This assumes that the file was written in the proper
        ;; place in the directory tree.
        year_dir = fix(dir_array[N_elements(dir_array)-2])
        day_dir = dir_array[N_elements(dir_array)-1]
        ;; Correct for new date format after 2000 (create a Y2100 bug!)
        if strmatch(day_dir, '20[0-9][0-9][0-9][0-9][0-9][0-9]') then $
           year_from_dir = fix(strmid(day_dir, 2, 2)) $
        else $
           year_from_dir = fix(strmid(day_dir, 0, 2))
        ;; The study started in 1990
        if year_from_dir ge 90 then begin
           year_from_dir = year_from_dir + 1900
        endif else begin
           year_from_dir = year_from_dir + 2000
        endelse
        year = string(year_dir)
        if year_from_dir ne year_dir then begin
           message, 'ERROR: directory ' + string(day_dir) + ' is filed in year ' + string(year_dir)
        endif
;        if year_fits ne year then begin
;           message, /CONTINUE, 'WARNING: the DATE suggests the FITS file was written ' + string(date) + ', but you are working in the ' + string(year_dir) + ' directory'
;        endif
        
        ;; MONTH 
        ;; some data directories are recorded 03jan01, others 970101.
        ;; Fri Feb  7 09:52:47 2014  jpmorgen@snipe
        ;; I just fixed this so before 2000, all directories are
        ;; YYMMDD.  After 2000, they are YYYY0101
        ;; Filenames before 1997 or so don't always have months in
        ;; them, so don't count on them.

        ;;month_from_file = 	strmid(shortinfile, 2, 3)
        ;;if strlen(day_dir) gt 6 then begin
        ;;   month_from_dir = 	strmid(day_dir, 2, 3)
        if strlen(day_dir) gt 6 then begin
           ;; > 2000
           temp = fix(strmid(day_dir, 4, 2))
           month_from_dir = months[temp-1]
        endif else begin
           temp = fix(strmid(day_dir, 2, 2))
           month_from_dir = months[temp-1]
        endelse
;        month = month_from_file
;        if month_from_file ne month_from_dir then begin
;           message, /CONTINUE, 'WARNING: month in raw directory tree (' + string(month_from_dir) + ') and filename (' + string(month_from_file) + ') do not agree.'
;           ;; If the above is commented out, lets assume that the
;           ;; directory tree year is correct.  We might want to
;           ;; rename the file too, but let's cross that hurdle if
;           ;; it happens
;           month = month_from_dir
;        endif
;        if month_fits ne month then begin
;           message, /CONTINUE, 'WARNING: the DATE suggests the FITS file was written ' + string(date) + ', but you are working in the ' + string(day_dir) + ' directory'
;        endif

        ;; Convert month back into an integer
        temp = where(months eq month_from_dir, count)
        if count eq 0 then $
          message, 'ERROR: bad month format ' + string(month_from_dir)
        imonth = temp[0] + 1
        ;; But with a leading 0
        if imonth lt 10 then begin
           month = string(format='("0", i1)', imonth)
        endif else begin
           month = strtrim(string(imonth),2)
        endelse

        ;; DAY
        if strlen(day_dir) gt 6 then begin
           ;; New format YYYYMMDD puts day starting in 6
           ;;day_from_dir = 	strmid(day_dir, 5, 2)
           day_from_dir = 	strmid(day_dir, 6, 2)
        endif else begin
           day_from_dir = 	strmid(day_dir, 4, 2)
        endelse
        day = day_from_dir

;        day_from_file = 	strmid(shortinfile, 0, 2)
;        day = day_from_file
;        if day_from_file ne day_from_dir then begin
;           message, /CONTINUE, 'WARNING: day in raw directory tree (' + string(day_from_dir) + ') and filename (' + string(day_from_file) + ') do not agree.'
;           ;; If the above is commented out, lets assume that the
;           ;; directory tree year is correct.  We might want to
;           ;; rename the file too, but let's cross that hurdle if
;           ;; it happens
;           day = day_from_dir
;        endif
;        if day_fits ne day then begin
;           message, /CONTINUE, 'WARNING: the DATE suggests the FITS file was written ' + string(date) + ', but you are working in the ' + string(day_dir) + ' directory'
;        endif

        date_obs = sxpar(hdr, 'DATE-OBS', count=date_count)
        if strtrim(date_obs, 2) eq '17/10/31' then begin
           message, /CONTINUE, 'WARNING: bogus date ' + strtrim(date_obs,2) + '.  Using DATE keyword instead'
           sxaddhist, string('(ssg_fix_head.pro) DATE-OBS keyword was bogus (see ODATEOBS)'), hdr
           sxaddhist, string('(ssg_fix_head.pro) replacing with DATE'), hdr
           sxaddpar, hdr, 'ODATEOBS', date_obs, 'Old, likely bogus DATE-OBS'
           
           date_obs = sxpar(hdr, 'DATE', count=count)
           ;; Change from - to / separators for code below
           datearr=strsplit(date_obs,'-',/extract)
           date_obs = datearr[0]+'/'+datearr[1]+'/'+datearr[2]
           sxaddpar, hdr, 'DATE-OBS', date_obs, 'Corrected DATE-OBS = DATE'
           if strtrim(ut, 2) eq '19:40:16.0' then begin
              message, /CONTINUE, 'WARNING: found a 960708 bias file.  Making up a unique UT'
              sxaddhist, string('(ssg_fix_head.pro) UT keyword is likely not unique (see OLD_UT)'), hdr
              sxaddhist, string('(ssg_fix_head.pro) ESTIMATED UT'), hdr
              sxaddpar, hdr, 'OLD_UT', ut, 'Old, non-unique UT'
              filenum = strmid(shortinfile, 3, 2)
              ut = '04:' + filenum + ':00.99'
              sxaddpar, hdr, 'UT', ut, 'ESTIMATED UT'
           endif
           bad_file = 1
        endif ;; Bogus DATE-OBS = 17/10/31
        ;; Telescope clock stuck
        if strtrim(date_obs, 2) eq '27/06/98' and $
          strtrim(ut, 2) eq '7:15:14.0' then begin
           message, /CONTINUE, 'WARNING: Estimating UT for ' + shortinfile + ' because telescope clock got stuck'
           sxaddhist, string('(ssg_fix_head.pro) UT keyword is likely not unique (see OLD_UT)'), hdr
           sxaddhist, string('(ssg_fix_head.pro) ESTIMATED UT'), hdr
           sxaddpar, hdr, 'OLD_UT', ut, 'Old, non-unique UT'
           filenum = strmid(shortinfile, 8, 2)
           timearr=strsplit(ut,':',/extract)
           ut = '07:' + strtrim(string(fix(timearr[1]) + $
                                       fix(filenum)-9), 2) + $
                ':00.99'
           sxaddpar, hdr, 'UT', ut, 'ESTIMATED UT'
           bad_file = 1
        endif
        if date_count eq 0 then begin
           message, 'WARNING: This seems to be an SSG file but it has no DATE-OBS keyword.  I will try to reconstruct one.', /CONTINUE
           date_obs = '00/00/00'
           sxaddhist, string('(ssg_fix_head.pro) DATE-OBS keyword was missing'), hdr
        endif ;; Telescope clock stuck

        datearr=strsplit(date_obs,'-T:',/extract)
        timearr=strsplit(ut,':',/extract)
        if N_elements(datearr) eq 6 then begin ; New Y2K convention
           year_fits = fix(datearr[0])
           month_fits = months[fix(datearr[1])-1]
           day_fits = fix(datearr[2])
        endif else begin        ; Old date format
           datearr=strsplit(date_obs,'/',/extract)
           if N_elements(datearr) ne 3 then begin
              message, /CONTINUE, 'WARNING: malformed DATE-OBS keyword'
              datearr = [0,0,0]
           endif

           year_fits = fix(datearr[2])
           ;; The study started in 1990
           if year_fits ge 90 then $
             year_fits = year_fits+1900 $
           else $
             year_fits = year_fits+2000
           year_fits = string(year_fits)
           month_fits = fix(datearr[1])
           day_fits = datearr[0]
           
           ;; Put a leading 0 on the month
           if month_fits lt 10 then begin
              month_fits = string(format='("0", i1)', month_fits)
           endif else begin
              month_fits = strtrim(string(month_fits),2)
           endelse
           
           date_obs = year_fits + '-' + month_fits + '-' + day_fits + 'T' + ut
           date_obs = strtrim(date_obs,2)
           sxaddpar, hdr, 'DATE-OBS', date_obs, 'Y2K compliant (yyyy-mm-ddThh:mm:ss)'
        endelse
        
        ;; Check date_obs against what the directory says
        constructed_date_obs = year + '-' + month + '-' + day + 'T' + ut
        constructed_date_obs = strtrim(constructed_date_obs,2)

        ;; There is at least one pathological case where there is no
        ;; DATE-OBS, but the biases were taken the day before (in UT
        ;; time).  
        if constructed_date_obs ne date_obs and fix(timearr[0]) gt 18 $
          and fix(day)-fix(day_fits) ne 1 then begin
           message, /CONTINUE, 'WARNING: I _think_ I found a bias with a bad DATE-OBS keyword that was taken in the previous UT.'
           if date_count ne 0 then begin
              sxaddhist, string('(ssg_fix_head.pro) DATE-OBS keyword modified, see ODATEOBS'), hdr
              sxaddpar, hdr, 'ODATEOBS', date_obs, 'Old DATE-OBS, either Y2K bugged or seconds rounded'
           endif
           sxaddpar, hdr, 'DATE-OBS', constructed_date_obs, 'Y2K compliant (yyyy-mm-ddThh:mm:ss)'
           bad_file = 1           
        endif ;; Bad date_obs on files taken in the previous UT day


        ;; Don't fix data that were taken in the previous UT day (UT
        ;; time > 18 or so), since that corresponds to mid-afternoon
        ;; biases, which were occationally taken
        if constructed_date_obs ne date_obs and fix(day)-fix(day_fits) ne 1 $
          and fix(timearr[0]) lt 18 then begin
           message, /CONTINUE, 'Construction of DATE-OBS keyword from directory and filename information yielded ' + string(constructed_date_obs) + ' which is different from FITS header value of ' + string(date_obs)

           if date_count ne 0 then begin
              sxaddhist, string('(ssg_fix_head.pro) DATE-OBS keyword modified, see ODATEOBS'), hdr
              sxaddpar, hdr, 'ODATEOBS', date_obs, 'Old DATE-OBS, either Y2K bugged or seconds rounded'
           endif
           sxaddpar, hdr, 'DATE-OBS', constructed_date_obs, 'Y2K compliant (yyyy-mm-ddThh:mm:ss)'
           bad_file = 1

        endif ;; Bad date_obs

        if keyword_set(bad_file) then begin
           message, /CONTINUE, 'Saving file ' + string(shortinfile) + ' in tar file ' + string(outname)
           create_or_append = 'r' ; --> I think this is SUN tar specific
           if num_bad eq 0 then create_or_append = 'c'
           spawn, 'tar '+create_or_append+'vf ' + outname + ' ' + shortinfile
           num_bad = num_bad + 1
           message, /INFORMATIONAL, 'Writing ' + outfile
;        ;; Code from writefits so that I have control over verbosity
;        ;; (no silent keyword for writefits)
;        check_fits, im, hdr, /UPDATE, /FITS, SILENT=silent
           writefits, outfile, im, hdr
        endif

     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL

  if keyword_set(num_bad) then begin
     message, /INFORMATIONAL, 'Compressing ' + string(outname)
     spawn, 'gzip ' + outname
  endif else begin
     message, /INFORMATIONAL, 'No bad SSG headers found.  No files in ' + indir + ' need to be modified'
  endelse

  return
end
