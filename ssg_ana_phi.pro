;+
; NAME: ssg_ana_phi
;
; PURPOSE: Put mooon orbital phase ephemeris information into the analysis database
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
; $Id: ssg_ana_phi.pro,v 1.1 2015/02/17 23:01:39 jpmorgen Exp $
;
; $Log: ssg_ana_phi.pro,v $
; Revision 1.1  2015/02/17 23:01:39  jpmorgen
; Initial revision
;
;-
pro ssg_ana_phi, $
   fnames, $
   begin_nday=begin_nday, $
   end_nday=end_nday

  ;; The begin_nday and end_nday should match the ssg_phi_req values
  ;; (if any)
  if N_elements(begin_nday) eq 0 then $
     begin_nday = 0d
  ;; Hopefully this is sufficiently beyond the end of the MMP lifetime
  if N_elements(end_nday) eq 0 then $
     end_nday = 1d9

  init = {ssg_sysvar}

  if N_elements(fnames) ne 4 then $
     message, 'ERROR: I need 4 files containing ephemeris information for the 4 Galilean satellites.  These should be the outnames of ssg_phi_req'

  ;; Prepare the list of columns we want.  --> If you change these,
  ;; change the code below which associates .col(n) with a particular column
  col_list = ['Date_________JDUT', $ ;; This is the time on earth when the light left the body
              'Ob-lon',  $ ;; sys III lon of moon
              'Ob-lat', $  ;; sub-Jovian lat (ref to dynamic equator) of moon
              'Sl-lon', $ ;; Jovian sub-solar point, for orbital phase
              'Sl-lat']
  
  ;; Read in our ephemerides one file at a time.  We don't know what
  ;; order the files are listed in, so we will read the observer
  ;; (moon) from the Subject line and file the ephemeris accordingly.
  ;; For our code below, we want to have things synchronized to the
  ;; obj_code in the reduction database.  oarr is indexed by the
  ;; obj_code (0=Jupiter, 1=Io, 2=Europa...) and contains values that
  ;; point to the eph element for that object.  oarr[0] = -1, since
  ;; here, we don't need Jupiter.
  oarr = make_array(5, value=-1)
  for ifi=0, 3 do begin
     ;; Read in an ephemeris file
     eph1 = eph_get_col(fnames[ifi], col_list=col_list, target=target, observer=observer)
     if target ne '599' then $
        message, 'ERROR: unexpected target ' + target + '.  I am expecting to have Jupiter (599) be the target, with each Galilean satellite being the observer in turn.'
     
     ;; Translate observer into obj_code (io=1, eur=2, etc.) , which
     ;; will be used to index the oarr
     obj_code = (strsplit(observer, '@', /extract))[1] - 500
     
     if obj_code lt 1 or obj_code gt 4 then $
        message, 'ERROR: unexpected observer ' + observer + '.  We expect things like 500@501, 500@502, etc., since we are looking at Jupiter from the perspective of the moons to get phases.'

     ;; Build up our ephemeris output.  This assumes that all HORIZONS
     ;; return files are the same, which should be reasonable, given
     ;; how they are requested.
     if N_elements(eph) eq 0 then $
        eph = eph1 $
     else $
        eph = [temporary(eph), eph1]

     ;; Make our association between our observing Galilean satellite
     ;; and the particular ephemeris we just read in
     oarr[obj_code] = ifi

  endfor ;; each ephemeris


  ;; Open our database in writable mode
  adbname = 'io_oi_analyze'
  oldpriv=!priv
  !priv = 2
  dbopen, adbname, 1

  ;; Get the database entries corresponding to the nday range we want.
  ;; Exclude bias, flat, etc. images by looking at "objects" only.
  ;; This is the same code we used in ssg_phi_req, to make sure we
  ;; have a 1-1 match to the ephemerides.  Note that this includes
  ;; entries for Jupiter...
  entries = dbfind(string("nday>", begin_nday), $
                   dbfind(string("nday<", end_nday)), count=ecount)
  if ecount eq 0 then begin
     message, /CONTINUE, 'WARNING: no objects in reduction database between ' + string(nday_range[0]) + ' and ' + string(nday_range[1])
     dbclose
     return
  endif

  ;; The nday values in the database are as observed on earth.
  ;; ssg_phe_req requested the calculations from the satellites, 1lt
  ;; before observed by the earth.  We need to get add back the 1lts
  ;; so that the ephemeris times in the files we just read can be
  ;; matched to ndays.  Also grab obj_code, so we can put the right
  ;; phi-related quantities in the generic phi columns
  dbext, entries, 'nday, obj_code, Io_1lt, Eur_1lt, Gan_1lt, Cal_1lt', $
         ndays, obj_codes, Io_1lts, Eur_1lts, Gan_1lts, Cal_1lts

  ;; Now populate the columns for each object in the database.
  ;; Columns exist for all ndays so as to make cross-correlations
  ;; easy.

  ;; Put together some mnemonics to make accessing the columns as a
  ;; function of object easy
  obj_names = ['Jup', 'Io', 'Eur', 'Gan', 'Cal']

  ;; Prepare the long_3, lat_eq, and phi columns.  These correspond to
  ;; the moon that is actually being observed (if any).  Initialize to
  ;; NAN and fill in below.  Note that Jupiter values will end up
  ;; being NAN
  long_3 = make_array(N_elements(ndays), value=!values.f_nan)
  lat_eq = long_3
  phi = long_3

  ;; Loop through each set of columns
  for ig=1,4 do begin
     ;; Get our light travel times, since the database is listed in
     ;; times at MMP and we had to request the observations of Jupiter
     ;; at the corresponding time at each satellite
     case ig of 
        1: lts = Io_1lts
        2: lts = Eur_1lts
        3: lts = Gan_1lts
        4: lts = Cal_1lts
     endcase
     ;; Convert our ndays to JD at the satellite so we can match the
     ;; HORIZONS value.  Note lts are stored in units of seconds in
     ;; the database.  Also note that order is important here: the
     ;; database is not necessarily sorted by nday, but HORIZONS does
     ;; sort tlist by time.  Convert ndays here and then find nday
     ;; matches, below
     jd_ndays = ndays - lts / (3600.*24.) + !ssg.JDnday0

     ;; Find nday matches
     for ind=0,N_elements(ndays)-1 do begin
        idx = where(jd_ndays[ind] - !ssg.tolerance lt eph[oarr[ig]].data.(0) and $
                    eph[oarr[ig]].data.(0) lt jd_ndays[ind] + !ssg.tolerance, count)
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
     
     ;; Update the phi-related columns in the analysis database for
     ;; this particular object.  m_ are for the particular moon we are
     ;; considering.
     m_III = eph[oarr[ig]].data[eidx].(1) ;; moon sys III lon
     m_lat = eph[oarr[ig]].data[eidx].(2) ;; moon equatorial latitude
     m_phi = eph[oarr[ig]].data[eidx].(3) - m_III + 180 ;; moon orbital phase 0 = pointing away from Sun
     ;; Make range of phi go from 0 to 360
     range_idx = where(m_phi lt 0., count)
     if count gt 0 then $
        m_phi[range_idx] += 360.
     range_idx = where(m_phi gt 360., count)
     if count gt 0 then $
        m_phi[range_idx] -= 360.
     
     ;; Find all the observations of the Galilean satellite we are
     ;; currently considering
     g_idx = where(obj_codes eq ig, count)
     ;; If any observations are found, copy the columns to our generic handles
     if count gt 0 then begin
        long_3[g_idx] = m_III[g_idx]
        lat_eq[g_idx] = m_lat[g_idx]
        phi[g_idx]    = m_phi[g_idx]
     endif

     ;; Write moon-specific phase arrays into the database
     dbupdate, entries, obj_names[ig]+'_long_3, ' + obj_names[ig]+'_lat_eq, ' + $
               obj_names[ig]+'_phi', float(m_III), float(m_lat), float(m_phi)

  endfor ;; Each set of satellite columns
  
  ;; For easy reference, make a separate "side" column.  There are
  ;; three choices: n.a. for non-Galilean satellites, east, and west
  side = make_array(N_elements(phi), value='n.a.')
  idx = where(phi le 180, count)
  if count gt 0 then $
     side[idx] = 'east'
  idx = where(phi gt 180, count)
  if count gt 0 then $
     side[idx] = 'west'

  ;; Now write the generic handles we have accumulated
  dbupdate, entries, 'long_3, lat_eq, phi, side', float(long_3), float(lat_eq), float(phi), side
               
  message, /INFORMATIONAL, 'Updated lon/lat/phi/side values in ' + adbname

  dbclose
  !priv=oldpriv

end
