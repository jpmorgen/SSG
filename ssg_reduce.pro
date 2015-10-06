;+
; $Id: ssg_reduce.pro,v 1.4 2015/03/04 15:50:16 jpmorgen Exp $

; Runs non-interactive SSG reduction

;-

pro exit_or_skip, interactive, exit_status=exit_status

  if NOT keyword_set(interactive) then RETURN

  for ki = 0,1000 do flush_input = get_kbrd(0)
  message, /CONTINUE, 'About to go to next step:'
  print, 'Continue to next step'
  print, 'Skip this directory'
  print, 'Quit/eXit, ending reduction for now'
  answer = ''
  for ki = 0,1000 do flush_input = get_kbrd(0)
  repeat begin
     message, /CONTINUE, '[C], S, Q/X?'
     answer = get_kbrd(1)
     if byte(answer) eq 10 then answer = 'C'
     for ki = 0,1000 do flush_input = get_kbrd(0)
     answer = strupcase(answer)
  endrep until $
     answer eq 'C' or $
     answer eq 'S' or $
     answer eq 'Q' or $
     answer eq 'X'

  if answer eq 'C' then begin
     RETURN
  endif ;; C

  if answer eq 'S' then begin
     message, 'Skipping '
  endif ;; S

  if answer eq 'Q' or answer eq 'X' then begin
     message, 'Stopping'
     ;; It would be more eligant to use exit status
     dbclose
     close, /all
     retall
  endif ;; S

end

pro ssg_reduce, indir, TV=TV, SHOW=SHOW, PLOT=PLOT, $
                interactive=interactive, _EXTRA=extra

;  ON_ERROR, 2

  if NOT keyword_set(indir) then indir = '/data/io/ssg/raw'
  noninteractive=0
  if NOT keyword_set(interactive) then noninteractive=1

  ;; Make sure we are a directory, but don't be noisy about it
  spawn, string('test -d ', indir), exit_status=status
  if status ne 0 then return

  dir_array = strsplit(indir, '/', /extract)
  depth = N_elements(dir_array)-1
  rawdepth = where(dir_array eq 'raw', count)
  if count eq 0 then $
     message, 'ERROR: input directory must be in the raw directory tree'

  ;; Discard test, ls-al, etc. and years beyond 2100 ;-)
  if depth - rawdepth eq 1 then begin
     if strlen(dir_array[depth]) ne 4 then return
     if strpos(dir_array[depth], '19') ne 0 $
        and strpos(dir_array[depth], '20') ne 0 then return
  endif

  ;; Some days have subdirectories with stuff in them.  Most of the
  ;; time this stuff is experimental reduction which doesn't belong in
  ;; the raw directory tree, so raise an error when one is detected.
  if depth - rawdepth gt 2 then begin
     message, /CONTINUE, 'ERROR: subdirectory ' + indir + ' detected.  Inspect this by hand to see what it really is.'
     return
  endif
  rawdir=indir
  reddir = ''
  dir_array[rawdepth] = 'reduced'
  for i=0, depth do begin
     reddir = string(reddir, '/', dir_array[i])
  endfor

  ;; Make sure reduced directory exists at this level and are group
  ;; lyra writable
  temp = file_search(reddir, count=count)
  if count eq 0 then begin
     spawn, string('mkdir ', reddir)
     spawn, string('chgrp lyra ', reddir), txtout, errout
     spawn, string('chmod g+w ', reddir), txtout, errout
  endif
  
  ;; Descend to the day dir level
  if depth - rawdepth le 1 then begin
     dirs = file_search(string(indir, '/*'))
     for i=0, N_elements(dirs)-1 do begin
        ssg_reduce, dirs[i], TV=TV, SHOW=SHOW, PLOT=PLOT
     endfor
     return
  endif

  ;; If we made it here, we are at the day dir level.
  ;; Turn on error catching so error messages show up in the log.  For
  ;; ease of checking, have ssg_reduce only produce output when there
  ;; is a genuine error
  CATCH, err
  if err ne 0 then begin
     message, /NONAME, !error_state.msg, /CONTINUE
     message, 'ERROR: skipping ' + indir, /CONTINUE
  endif else begin

     write = noninteractive
     ssg_fix_head, rawdir
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_db_init, rawdir, /APPEND, /DELETE, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_raw_cp, rawdir, reddir, /OVERWRITE, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_get_overclock, reddir, TV=TV, write=write, $
                        noninteractive=noninteractive, $
                        _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_biasgen, reddir, TV=TV, PLOT=PLOT, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_biassub, reddir
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_get_sliloc, reddir, TV=TV, SHOW=SHOW, PLOT=PLOT, write=write, $
                     noninteractive=noninteractive, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_fit_sliloc, reddir, noninteractive=noninteractive, write=write, _EXTRA=extra ;;, $
     ;; The slicer location is now well determined
     ;;                flat_only=noninteractive
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_get_camrot, reddir, TV=TV, SHOW=SHOW, write=write, $
                     noninteractive=noninteractive, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_fit_camrot, reddir, noninteractive=noninteractive, write=write, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_lightsub, reddir, TV=TV,SHOW=SHOW, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_flatgen, reddir, TV=TV,SHOW=SHOW, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_flatfield, reddir, TV=TV, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_get_slicer, reddir, noninteractive=noninteractive, $
                     write=write, TV=TV, SHOW=SHOW, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_fit_slicer, reddir, noninteractive=noninteractive, write=write, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_get_dispers, reddir, PLOT=PLOT, TV=TV, show_first_pass=SHOW, $
                      noninteractive=noninteractive, write=write, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_fit_dispers, reddir, noninteractive=noninteractive, write=write, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     

     ssg_cr_mark, reddir, cutval=5, TV=TV, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_cr_replace, reddir, TV=TV, _EXTRA=extra
     exit_or_skip, interactive, exit_status=exit_status     
     ssg_extract, reddir, SHOW=SHOW, TV=TV, noninteractive=noninteractive, write=write, _EXTRA=extra
     if keyword_set(interactive) then $
        message, /CONTINUE, 'Finished with ' + reddir
     exit_or_skip, interactive, exit_status=exit_status     
  endelse

  CATCH, /CANCEL
  ;; Clean up after any messes errors may have left. (actually there is
  ;; an open file problem floating around, which ssg_extract probably
  ;; fixes, but when running at this level, that is sometimes skipped
  ;; due to errors (e.g. /home/jpmorgen/data/ssg/raw/1994/940724
  dbclose
  close, /all
end

