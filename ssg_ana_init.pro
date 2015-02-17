;+
; NAME: ssg_adb_init
;
; PURPOSE: initialize the analysis database
;
; CATEGORY: Io [OI] fluxes
;
; CALLING SEQUENCE: 
;
; DESCRIPTION:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 

; delete: delete entries from database so they can be overwritten with
; new versions
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
; $Id: ssg_ana_init.pro,v 1.1 2015/02/17 23:00:52 jpmorgen Exp $
;
; $Log: ssg_ana_init.pro,v $
; Revision 1.1  2015/02/17 23:00:52  jpmorgen
; Initial revision
;
;-
pro ssg_ana_init, $
   begin_nday=begin_nday, $
   end_nday=end_nday, $
   delete=delete, $
   append=append

  init = {ssg_sysvar}

  if N_elements(begin_nday) eq 0 then $
     begin_nday = 0d
  ;; Hopefully this is sufficiently beyond the end of the MMP lifetime
  if N_elements(end_nday) eq 0 then $
     end_nday = 1d9

  ;; Open up our reduction database
  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0

  ;; Get the database entries corresponding to the nday range we want.
  ;; Exclude bias, flat, etc. images by looking at "objects" only with
  ;; obj_code le 4 only (Jupiter through Callisto).
  ;; Exclude anything marked "bad"
  entries = dbfind(string("nday>", begin_nday), $
                   dbfind(string("nday<", end_nday), $
                          dbfind(string("obj_code<", 4), $
                                 dbfind(string("typecode=", 5), $
                                        dbfind(string("bad=0"))))), count=icount)
  if icount eq 0 then begin
     message, /CONTINUE, 'WARNING: no good objects in reduction database'
     dbclose
     return
  endif

  ;; Read the information we want to transfer over to the analysis database
  dbext, entries, 'date, time, nday, object, obj_code, fname, exptime, n_disp, n_xdisp', dates, times, ndays, objects, obj_codes, fnames, exptimes, n_disps, n_xdisps

  dbclose

  ;; Get ready to put this information into our analysis database.
  ;; First, check to see if we have any duplicates 
  adbname = 'io_oi_analyze'
  dbopen, adbname, 0

  ;; Find all entries in the analysis database
  entries = dbfind("nday>0", count=acount)
  ;; dbget doesn't like an empty database 
  ;;entries = dbget('nday', ndays, count=acount)

  if acount ge 1 then begin
     message, /CONTINUE, 'NOTE: reinitializing entries in analysis database'
     delete_list = entries
  endif

  opriv = !priv
  ;; DO DATABASE DELETES
  if N_elements(delete_list) gt 0 then begin
     if !priv ge 3 then begin
        message, /CONTINUE, 'WARNING, !priv set to ' + string(!priv) + ', continuing automatically with database deletes'
     endif 
     if keyword_set(delete) then !priv = 3
     if !priv lt 3 then message, 'ERROR: deletions from the database need to be made, so !priv needs to be at least 3.  Alternately (and preferred), you can specify the /DELETE option to this procedure.  Hint, you will probably want /APPEND too'
     dbdelete, delete_list, adbname
     ;; Thu Jan 27 13:42:24 2011  jpmorgen
     ;;message, /INFORMATIONAL, 'deleted items marked'
     ;; Latest version of dbdelete does not need dbcompress
     ;;dbcompress, dbname
     ;; Quietly change the mode and group of the file to be group lyra
     ;; writable so other people can work on this
     adbfname = find_with_def(adbname + '.dbf','ZDBASE')
     spawn, string('chmod g+w ', adbfname), txtout, errout
     ;;spawn, string('chgrp lyra ', adbfname), txtout, errout
     message, /INFORMATIONAL, 'items deleted removed';;, database group lyra writable'
     dbclose
     message, /INFORMATIONAL, 'database closed'
  endif

  !priv = opriv

  ;; DO DATABASE APPENDS
  if !priv ge 2 then begin
     message, /CONTINUE, 'WARNING, !priv set to ' + string(!priv) + ', continuing automatically with database appends'
  endif 
  if keyword_set(append) then $
     !priv = 2
  if !priv lt 2 then message, $
     'ERROR: appends to the database need to be made, so !priv needs to be at least 2.  Alternately (and preferred), you can specify the /APPEND option to this procedure.'

  get_date,today

  dbopen, adbname, 1

  ;; dbbuilt puts everything in at once and depends on all the bits
  ;; being aligned
  dbbuild, dates, $
           times, $
           ndays, $
           objects, $
           obj_codes, $
           make_array(icount, value=''), $
           make_array(icount, value=0), $
           fnames, $
           make_array(icount, value=''), $
           exptimes, $
           n_disps, $
           n_xdisps, $
           make_array(icount, value=today)

  dbclose

  !priv=opriv

end
