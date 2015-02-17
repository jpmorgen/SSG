;+
; NAME: ssg_fix_dirs
;
; PURPOSE: fix directory enties in database after move to midnight1
;
; CATEGORY: SSG
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: ssg_fix_dirs.pro,v 1.1 2015/02/17 23:03:29 jpmorgen Exp $
;-
pro ssg_fix_dirs
  init = {ssg_sysvar}
  dbname = 'ssg_reduce'
  dbopen, dbname, 0 
  entries = dbfind('nday > 0')
;;  entries = indgen(10)
  dbext, entries, "nday, raw_dir, dir", ndays, raw_dirs, dirs
  dbclose

  for i=0,N_elements(entries)-1 do begin
     ;; raw_dir
     old_dir = strsplit(raw_dirs[i], '/', /extract, count=count)
     if count lt 4 then begin
        message, /CONTINUE, 'WARNING: wierd directory "' + strtrim(old_dir, 2) + '" found at nday = ' + strtrim(ndays[i], 2)
        CONTINUE
     endif ;; wierd dir
     new_dir = '/data/io/' + $
       strjoin(string(format='(a, :, "/")', old_dir[3:N_elements(old_dir)-1]))
     raw_dirs[i] = new_dir
     ;;print, new_dir
     ;; dir
     old_dir = strsplit(dirs[i], '/', /extract, count=count)
     if count lt 4 then begin
        message, /CONTINUE, 'WARNING: wierd directory "' + strtrim(old_dir, 2) + '" found at nday = ' + strtrim(ndays[i], 2)
        CONTINUE
     endif ;; wierd dir
     new_dir = '/data/io/' + $
       strjoin(string(format='(a, :, "/")', old_dir[3:N_elements(old_dir)-1]))
     dirs[i] = new_dir
  endfor

  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'raw_dir, dir', raw_dirs, dirs
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated slicer parameters in ' + dbname


end
