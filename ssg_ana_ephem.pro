;+
; NAME: ssg_ana_ephem
;
; PURPOSE: Put basic ephemeris information into the analysis database
;
; CATEGORY:
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
; $Id: ssg_ana_ephem.pro,v 1.1 2015/02/17 23:00:27 jpmorgen Exp $
;
; $Log: ssg_ana_ephem.pro,v $
; Revision 1.1  2015/02/17 23:00:27  jpmorgen
; Initial revision
;
;-
pro ssg_ana_ephem, $
   fnames, $
   begin_nday=begin_nday, $
   end_nday=end_nday

  init = {ssg_sysvar}

  if N_elements(fnames) ne 5 then $
     message, 'ERROR: I need 5 files containing ephemeris information for the 5 major bodies in the Jovian system.  These should be the outnames of ssg_eph_req'

  ;; The begin_nday and end_nday should match the ssg_phi_req values
  ;; (if any)
  if N_elements(begin_nday) eq 0 then $
     begin_nday = 0d
  ;; Hopefully this is sufficiently beyond the end of the MMP lifetime
  if N_elements(end_nday) eq 0 then $
     end_nday = 1d9

  ;; Prepare the list of columns we want.  --> If you change these,
  ;; change the code below which associates .col(n) with a particular column
  col_list = ['Date_________JDUT', $
              'R.A._(ICRF/J2000.0)', '' + $ ;; These depend on J2000 being the desired coordinate system
              'DEC_(ICRF/J2000.0)', $
              'Elev_(a-app)', $
              'L_Ap_Sid_Time', $ ;; hour angle
              'a-mass', $
              'Ang-diam', $ ;; arcsec
              'Ob-lon',  $ ;; planetographic sub-observer point
              'Ob-lat', $
              'r', $
              'rdot', $
              'delta', $
              'deldot', $
              '1-way_LT', $
              'S-T-O' $ ;; solar phase angle
             ]
  
  ;; Read in our ephemerides one file at a time.  We don't know what
  ;; order the files are listed in, so we will use information in each
  ;; file to figure out which target is which.  For our code below, we
  ;; want to have things synchronized to the obj_code in the reduction
  ;; database.  tarr is indexed by the obj_code (0=Jupiter, 1=Io,
  ;; 2=Europa...) and contains values that point to the eph element
  ;; for that object.
  tarr = make_array(5, value=-1)
  for ifi=0, 4 do begin
     ;; Read in an ephemeris file
     eph1 = eph_get_col(fnames[ifi], col_list=col_list, target=target, observer=observer)
     ;; observer is returned as usr={248.405325,31.9584167,2.06675^G}@399
     if (strsplit(observer, '@', /extract))[1] ne '399' then $
        message, 'ERROR: unexpected observing body.  This is for the Stellar Spectrograph at the McMath-Pierce solar telescope, on earth.  Observatory code should be 695@399'
     
     ;; Translate target into obj_code, which will be used as an index
     ;; in the code below
     if target eq 599 then $
        target = 500
     target -= 500
     if target lt 0 or target gt 4 then $
        message, 'ERROR: unexpected target code ' + strtrim(target+500, 2)

     ;; Build up our ephemeris output.  This assumes that all HORIZONS
     ;; return files are the same, which should be reasonable, given
     ;; how they are requested.
     if N_elements(eph) eq 0 then $
        eph = eph1 $
     else $
        eph = [temporary(eph), eph1]

     ;; Make our association between our target and the particular
     ;; ephemeris we just read in
     tarr[target] = ifi

  endfor ;; each ephemeris

  adbname = 'io_oi_analyze'

  ;; Prepare a mnemonic for 1lt column names
  obj_lts = ['Jup_1lt', 'Io_1lt', 'Eur_1lt', 'Gan_1lt', 'Cal_1lt']
  
  oldpriv=!priv
  !priv = 2
  dbopen, adbname, 1

  ;; Store our ephemeris items in the analysis database one object at
  ;; a time.
  for iobj=0,4 do begin
     message, /INFORMATIONAL, 'NOTE: processing ' + !ssg.obj_codes[iobj]
     ;; Find the entries in the analysis database corresponding to this object
     entries = dbfind(string("nday>", begin_nday), $
                   dbfind(string("nday<", end_nday), $
                          dbfind(string('obj_code=', iobj))), count=ecount)
     ;; Skip this object if it has not been observed
     if ecount eq 0 then $
        CONTINUE

     ;; Extract the ndays for this object so that we can grab only
     ;; those ndays from the ephemeris file
     dbext, entries, 'nday', ndays

     ;; Find nday matches
     for ind=0,N_elements(ndays)-1 do begin
        ;; Convert JD to nday.  Recall that there are 5 eph,
        ;; accumulated above.  We built up tarr to index these.
        eph_nday = eph[tarr[iobj]].data.(0) - !ssg.JDnday0
        ;; find index into ephemeris
        idx = where(ndays[ind] - !ssg.tolerance lt eph_nday and $
                    eph_nday lt ndays[ind] + !ssg.tolerance, count)
        if count ne 1 then $
           message, 'ERROR: ' + strtrim(count, 2) + ' matches found between ephemeris file and analysis database for nday ' + strtrim(ndays[ind], 2)
        ;; If we made it here, we have found our one and only one
        ;; match between the requested and retrieved ephemeris for
        ;; this object.  Accumulate these idx, which index the
        ;; ephemeris columns.  Make sure to reset eidx for each iobj
        if ind eq 0 then $
           eidx = idx $
        else $
           eidx = [temporary(eidx), idx]
     endfor ;; each db entry for this object
     
     ;; Update the ephemeris columns in the analysis database for this
     ;; particular object

     ;; Could do a more robust search for the column name to column
     ;; number associations using eph.col_names, but that seems
     ;; overkill, since col_list is right above.
     dbupdate, entries, 'ra, dec, zd, ha', $
               float(eph[tarr[iobj]].data[eidx].(1)), float(eph[tarr[iobj]].data[eidx].(2)), $
               float(90. - eph[tarr[iobj]].data[eidx].(3)), float(eph[tarr[iobj]].data[eidx].(4))
     dbupdate, entries, 'am, ang_dia, ob_lon, ob_lat', $
               float(eph[tarr[iobj]].data[eidx].(5)), float(eph[tarr[iobj]].data[eidx].(6)), $
               float(eph[tarr[iobj]].data[eidx].(7)), float(eph[tarr[iobj]].data[eidx].(8))
     dbupdate, entries, 'r, rdot, delta, deldot', $
               eph[tarr[iobj]].data[eidx].(9), float(eph[tarr[iobj]].data[eidx].(10)), $
               eph[tarr[iobj]].data[eidx].(11), float(eph[tarr[iobj]].data[eidx].(12))
     ;; NOTE: 1lt is in minutes in HORIZONS, but I prefer to keep it
     ;; in s in the database
     dbupdate, entries, '1lt, spa', $
               eph[tarr[iobj]].data[eidx].(13)*60d, float(eph[tarr[iobj]].data[eidx].(14))

     ;; Now update the 1-way light times columns.  Note we are looping
     ;; over the column (i1lt_obj) but using the entries and eidx for
     ;; the rows.  Eventually all rows and columns will get filled
     ;; even if individual moons have not been observed.  Also note
     ;; translation from min (HORIZONS convention) to s (database
     ;; convention)
     for i1lt_obj=0,4 do begin
        dbupdate, entries, obj_lts[i1lt_obj], eph[tarr[i1lt_obj]].data[eidx].(13)*60d
     endfor

     message, /INFORMATIONAL, 'Updated ephemeris values for ' + !ssg.obj_codes[iobj] + ' in ' + adbname

  endfor ;; Each object

  dbclose
  !priv=oldpriv

end
