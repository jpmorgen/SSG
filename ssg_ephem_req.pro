;+
; NAME: ssg_ephem_req
;
; PURPOSE: generate file to request ephemerides for observations in
; the ssg reduction database
;
; CATEGORY: ssg reduction/analysis
;
; CALLING SEQUENCE: ssg_ephem_req, obj, [begin_nday, [end_nday,]], [/nomail]
;   begin_nday=begin_nday, $
;   end_nday=end_nday, $
;   nomail=nomail, $
;   fname=fname, $
;   outnames=outnames, $
;   timeout=timeout
;
; DESCRIPTION: creates a JPL HORIZONS batch job request for
; observations stored in the ssg reduction database.  Sends the file
; to HORIZONS unless /nomail is set
;
; INPUTS: 

; obj:

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
; RESTRICTIONS: If HORIZONS requests are being issued by more than one
; process, the emails might get confused
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: ssg_ephem_req.pro,v 1.2 2015/03/04 15:49:37 jpmorgen Exp $
;
; $Log: ssg_ephem_req.pro,v $
; Revision 1.2  2015/03/04 15:49:37  jpmorgen
; Summary: Last checkin before git
;
; Revision 1.1  2014/01/31 20:03:47  jpmorgen
; Initial revision
;
;-
pro ssg_ephem_req, $
   begin_nday=begin_nday, $
   end_nday=end_nday, $
   nomail=nomail, $
   fname=fname, $
   outnames=outnames, $
   timeout=timeout

  init = {ssg_sysvar}

  if NOT keyword_set(fname) then $
     fname = !ssg.top + '/analysis/ephemerides/ssg_ephem_req.txt'
  
  if N_elements(begin_nday) eq 0 then $
     begin_nday = 0d
  ;; Hopefully this is sufficiently beyond the end of the MMP lifetime
  if N_elements(end_nday) eq 0 then $
     end_nday = 1d9

  ;; Open up our reduction database
  dbclose ;; Just in case
  adbname = 'io_oi_analyze'
  dbopen, adbname, 0

  ;; Get the database entries corresponding to the nday range we want.
  ;; Exclude anything marked "bad"
  entries = dbfind(string("nday>", begin_nday), $
                   dbfind(string("nday<", end_nday)), count=ecount)
  if ecount eq 0 then begin
     message, /CONTINUE, 'WARNING: no objects in reduction database between ' + string(nday_range[0]) + ' and ' + string(nday_range[1])
     dbclose
     return
  endif

  ;; Get our ndays, which is all we need to generate the ephemeris tlist
  dbext, entries, 'nday', ndays

  dbclose

  ;; Set our command to get all big bodies in Jovian system at once.
  ;; This returns a single email for each body.  Prepare to keep track
  ;; of them in eph_unix_mail_req
  command = "'501' '502' '503' '504' '599'"
  N_new_files = N_elements(strsplit(command, ' '))
  ;;eph_table_req, fname=fname, command=command, $
  ;;               center='695@399', $ ;; Kitt Peak on Earth
  ;;               tlist=!ssg.JDnday0 + ndays ;; convert ndays to JD
  eph_table_req, fname=fname, command=command, $
                 center='coord@399', $ ;; prepare to enter precise coordinates of MMP (accruate to about 1m)
                 coord_type='GEODETIC', $
                 site_coord=!ssg.mmp_geodetic, $
                 tlist=!ssg.JDnday0 + ndays ;; convert ndays to JD

  ;; Unless explicitly asked not to, email our job to HORIZONS.  See
  ;; documentation in eph_unix_mail_req to learn how to use procmail
  ;; to get HORIZONS emails to go to MH_dir.  NOTE: our one ephemeris
  ;; request will generate five files
  if NOT keyword_set(nomail) then begin
     eph_unix_mail_req, fnames=fname, $
                        outnames=outnames, $
                        timeout=timeout, $
                        MH_dir=!ssg.top + '/analysis/ephemerides', $
                        N_new_files=N_new_files
  endif
end
