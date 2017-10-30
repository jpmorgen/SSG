; +
; $Id: ssg_sysvar__define.pro,v 1.2 2015/03/03 19:50:53 jpmorgen Exp $

; ssg_sysvar__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  So call explicitly with an
; argument if you need to have a default structure with different
; initial values, or as in the case here, store the value in a system
; variable.

;; This defines the !ssg system variable, which contains global
;; variables handy in processing the SSG data.

; -

pro ssg_sysvar__define, top
  ;; Handle the annoying system variables(s) used in the db system.
  ;; It is unfortunate that these aren't automatically defined upon
  ;; compile of those programs.
  defsysv, '!priv', exists=exists
  if exists eq 0 then $
    stis_startup

  ;; System variables cannot be redefined and named structures cannot
  ;; be changed once they are defined, so it is OK to check this right
  ;; off the bat.
  defsysv, '!ssg', exists=exists
  if exists eq 1 then begin
     if keyword_set(top) then $
       !ssg.top = top
     return
  endif
  
  if N_elements(top) eq 0 then $
    top = '/data/io/ssg'

  ;; Read in and customize the sso and eph system variables.  Make
  ;; sure this is after the bailout point, or else the customizations
  ;; won't stick.
  init = {sso_sysvar}
  init = {eph_sysvar}
  ;; Set the minimum number of lines needed to start plotting a
  ;; Doppler shifted axis to 3 so Io lines don't trigger an axis.  Use
  ;; !sso.special to indicate Io lines instead.
  !sso.min_lines = 3
  ;; --> work on this plot proc will hopefully get better...
  ;;!pfo.plotproc = 'sso_plot_fit'

  ;; getting ready to change over to new dynamic pfo system -- later
  ssg_parinfo__define, parinfo=ssg_parinfo
  ssg_dg_struct__define, dg_struct=dg_struct
  ssg $
     = {ssg_sysvar, $
        top	:	top, $
        $;; Create a list of regexp that we know raise frequent errors
        non_fits : ['ORIG*', '*.tex', '*.lis', '*.log', '*.ps', '*.dvi', '*.pdf'], $

        JDnday0	:	julday(1,1,1990,0), $ ; JDUTC of nday = 0
       $ ;; The smallest tolerance that dbfind could handle was 0.00005
       $ ;; using f11.5 input formatting.  That was about 4.3 s, which 
       $ ;; is way faster than files should be coming from the TI chips.  
       $ ;; Something seeems to be rounding on some input file, so increase
       $ ;; the tolerance --> a faster camera might need a smaller tolerance 
       $ ;; and a different formatted input to dbfind
       tolerance:	0.00005d*2d, $
       $ ;; For prett-printing objects from obj_codes in database.
       obj_codes: ['Jupiter', 'Io', 'Europa', 'Ganymede', 'Callisto', 'Night Sky', 'Dark', 'Comp', 'Flat', 'Day Sky', 'Other'], $
       $ ;; LONGITUD= '-111:35:40.83'      / Longitude (ddd:mm:ss.ss) - means West)
       $ ;; LATITUDE= '31:57:30.3'         / Latitude (dd:mm:ss.s)
       $ ;; ALTITUDE=                 2096 / Altitude (Meters)                              
        $ ;; mmp_xyz is the position of MMP relative to the center of 
       $ ;; the earth in the ITRF93 reference frame.  See 
       $ ;; ~/data/ephemeris/tk/mmp.tk (file lost) for details about how it was 
       $ ;; derived.  Basically, the topocentric coordinates came from
       $ ;; http://nsokp.nso.edu/mp/info.html (used to be site.html) the correction for
       $ ;; the geoid height (basis of altitude) vs. height above
       $ ;; the ellipsoid came from 
       $ ;; http://www.ngs.noaa.gov/CORS/utilities4/ (lost), but https://www.unavco.org/software/geodetic-utilities/geoid-height-calculator/geoid-height-calculator.html might do and the conversion
       $ ;; to x,y,z from http://www.ngs.noaa.gov/cgi-bin/xyz_getxyz.prl
       mmp_xyz	:	[-1994124.0333d, $
                         -5037950.3038d, $
                         3357614.1124d] / 1000d, $
        c : 299792.458d, $ ;; speed of like in km/s
        TIo: 1.769137786d , $ ;; Io's orbital period in days
       $ ;; The mmp.tk file was lost, but io.notebk from April 26, 2004 and Feb 12, 2013 has notes
       $ ;; mmg_geodetic is just a string that gets sent on to the HORIZONS email interface in ssg_ephem_req
       mmp_geodetic : '248.405325000, 31.958416667, 2.06675', $
       min_cont	:	5, $ ; e/s minimum continuum value for fit
       $ ;; --> Eventually, I want to do this better
       obj_lines	:	[5577.34d, 5889.95d, 5895.92d, 6300.304d], $
       lparinfo	:	ptr_new(), $
       $ ;; A psuedo random number for parinfo.pfo.status equivalent width
       $ ;; stuff in ssg_fit1spec
       too_small 	:	-873, $
       dg_struct	:	dg_struct, $
       parinfo		:	ssg_parinfo}

  defsysv, '!ssg', ssg

end


