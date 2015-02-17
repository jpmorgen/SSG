;+
; NAME: ssg_phi_req
;
; PURPOSE: generate file to request ephemerides of Jupiter from the
; perspectives of the body centers of the Galilean satellites.  
;
; CATEGORY: ssg reduction/analysis
;
; CALLING SEQUENCE: ssg_ephem_req, obj, [begin_nday, [end_nday,]], [/nomail]
;
; DESCRIPTION: creates a JPL HORIZONS batch job request for
; observations stored in the ssg reduction database by ssg_db_init,
; ssg_ephem_req, and ssg_ana_ephem.  Sends the file
; to HORIZONS unless /nomail is set.
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
; RESTRICTIONS: If HORIZONS requests are being issued by more than one
; process, the emails might get confused
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
; $Id: ssg_phi_req.pro,v 1.1 2015/02/17 23:01:22 jpmorgen Exp $
;
; $Log: ssg_phi_req.pro,v $
; Revision 1.1  2015/02/17 23:01:22  jpmorgen
; Initial revision
;
;-
pro ssg_phi_req, $
   begin_nday=begin_nday, $
   end_nday=end_nday, $
   nomail=nomail, $
   fnames=fnames, $
   outnames=outnames, $
   timeout=timeout

  init = {ssg_sysvar}

  ;; Set up an individual file name for each Galilean satellite
  if NOT keyword_set(fname) then begin
     fnames = !ssg.top + '/analysis/ephemerides/ssg_phi_req_' + !ssg.obj_codes[1:4] + '.txt'
  endif
  
  if N_elements(begin_nday) eq 0 then $
     begin_nday = 0d
  ;; Hopefully this is sufficiently beyond the end of the MMP lifetime
  if N_elements(end_nday) eq 0 then $
     end_nday = 1d9

  ;; Open up our analysis database
  dbclose ;; Just in case
  adbname = 'io_oi_analyze'
  dbopen, adbname, 0

  ;; Get the database entries corresponding to the nday range we want.
  ;; Exclude bias, flat, etc. images by looking at "objects" only.
  ;; Note that this includes Jupiter, even though we are only
  ;; interested in the moons...
  entries = dbfind(string("nday>", begin_nday), $
                   dbfind(string("nday<", end_nday)), count=ecount)
  if ecount eq 0 then begin
     message, /CONTINUE, 'WARNING: no objects in analysis database between ' + string(nday_range[0]) + ' and ' + string(nday_range[1])
     dbclose
     return
  endif

  ;; Get our ndays and 1LTs
  dbext, entries, 'nday, Jup_1lt, Io_1lt, Eur_1lt, Gan_1lt, Cal_1lt', ndays, Jup_1lts, Io_1lts, Eur_1lts, Gan_1lts, Cal_1lts
  dbclose

  ;; Set up common 
  command = '599'

  ;; Loop through each of the Galilean satellites to generate an
  ;; ephemeris request file with the satellite as the center
  for ig=1,4 do begin
     case ig of
        1: lts = Io_1lts
        2: lts = Eur_1lts
        3: lts = Gan_1lts
        4: lts = Cal_1lts
     endcase
     ;; Convert LTs to days
     lts /= 3600.*24.
     eph_table_req, fname=fnames[ig-1], command=command, $
                    center='500@' + strtrim(500+ig, 2), $ ;; center of each moon
                    tlist=!ssg.JDnday0 + ndays - lts ;; convert ndays to JD and subtract LTs
  endfor ;; each moon

  ;; Set our command to get all big bodies in Jovian system at once.
  ;; This returns a single email for each body.

  ;; Unless explicitly asked not to, email our job to HORIZONS.  See
  ;; documentation in eph_unix_mail_req to learn how to use procmail
  ;; to get HORIZONS emails to go to MH_dir
  if NOT keyword_set(nomail) then begin
     N_new_files = 4 ;; one returned file for each moon
     eph_unix_mail_req, fnames=fnames, $
                        outnames=outnames, $
                        timeout=timeout, $
                        MH_dir=!ssg.top + '/analysis/ephemerides', $
                        N_new_files=N_new_files
  endif ;; sending mail to HORIZONS
end
