;+
; NAME:
;
; PURPOSE:
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
; $Id: ssg_blob_plot.pro,v 1.8 2015/03/04 15:44:58 jpmorgen Exp $
;
; $Log: ssg_blob_plot.pro,v $
; Revision 1.8  2015/03/04 15:44:58  jpmorgen
; Summary: Last checkin before git
;
; Revision 1.7  2015/01/14 00:30:36  jpmorgen
; Tweaked a bit
;
; Revision 1.6  2014/11/28 15:57:56  jpmorgen
; Added some histogram stuff I am still working on
;
; Revision 1.5  2014/11/14 17:53:43  jpmorgen
; Changing dots
;
; Revision 1.4  2014/11/07 14:04:16  jpmorgen
; Perhaps a more unbias census
;
; Revision 1.3  2014/11/06 02:47:58  jpmorgen
; Getting ready to play with points some more
;
; Revision 1.2  2014/11/04 00:08:32  jpmorgen
; Entered in lots of info!
;
; Revision 1.1  2014/11/01 02:18:36  jpmorgen
; Initial revision
;
;-
pro ssg_blob_plot, sysIV=sysIV, sys42=sys42, zero=zero, phased=phased, ps=ps, $
                   N_freq=N_freq, time_accuracy_minutes=time_accuracy_minutes, $
                   _EXTRA=extra

  init = {tok_sysvar}
  ;; t = findgen(100)/10.* 3600.*24
  ;; sysIII_ang = t/sysIII*360
  ;; cirrange, sysIII_ang
  ;; print, sysIII_ang

  ;; Initialize postscipt output
  if keyword_set(ps) then begin
     ;; Be polite, but get the line thicknesses we need for PS output
     oPthick     = !P.thick
     oPcharsize  = !P.charsize
     oPcharthick = !P.charthick
     oXthick     = !X.thick
     oYthick     = !Y.thick

     !P.thick = 3
     !P.charsize = 1.5
     !P.charthick = 2
     !X.thick = 2
     !Y.thick = 2      
     if size(ps, /TNAME) ne 'STRING' then $
       ps = 'ssg_blop_plot_out.eps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /encap
  endif

  ;; Find points in sequence of data taken starting 1998-10-1, which is shown in Oliversen et al. (2001)
  dbopen,'io6300_integrated'
  entries = dbfind("obj_code=1", dbfind("intensity>0.05", $
                   dbfind("nday>3195", dbfind("nday<3220"))))
  dbext, entries, 'nday, LONG_3, intensity, err_intensity', $
         mndays, mLONG_3, mintensities, merr_intensities
  dbclose
  dayo0 = 3194.
  dayo = mndays-dayo0

  ;; Generate finding table
  cirrange, mlong_3
  for i=0,N_elements(dayo)-1 do print, dayo[i], mlong_3[i]

  ;; Make plot from data table
  plot, [0,0], psym=dot, $
        xrange=[1,11], yrange=[-10,720], $
        ytickinterval=90, yminor=9, $
        xstyle=!tok.exact, ystyle=!tok.exact, $
        xtitle='!6 Day of 1998-10', $
        ytitle='Io System III Longitude (deg)'

  ;; Generate table of events
  dayob =         1.18384  & sysIIIb =           117.951  & psymb =         !tok.square    ;; single point possibly hight
  dayob = [dayob, 1.21680] & sysIIIb = [sysIIIb, 139.875] & psymb = [psymb, !tok.square  ] ;; single point
  dayob = [dayob, 1.33887] & sysIIIb = [sysIIIb, 221.322] & psymb = [psymb, !tok.square  ] ;; broad peak 
  dayob = [dayob, 1.39355] & sysIIIb = [sysIIIb, 257.749] & psymb = [psymb, !tok.square  ] ;; decent single point statistic
  dayob = [dayob, 1.45996] & sysIIIb = [sysIIIb, 302.113] & psymb = [psymb, !tok.square  ] ;; broad peak
  dayob = [dayob, 5.16846] & sysIIIb = [sysIIIb, 255.749] & psymb = [psymb, !tok.square  ] ;; follow model
  dayob = [dayob, 5.21216] & sysIIIb = [sysIIIb, 284.925] & psymb = [psymb, !tok.triangle] ;; follow model
  dayob = [dayob, 5.29712] & sysIIIb = [sysIIIb, 341.479] & psymb = [psymb, !tok.triangle] ;;
  dayob = [dayob, 5.34082] & sysIIIb = [sysIIIb,  10.634] & psymb = [psymb, !tok.square  ] ;; two points
  dayob = [dayob, 5.38232] & sysIIIb = [sysIIIb, 38.4062] & psymb = [psymb, !tok.square  ] ;; single point?
  dayob = [dayob, 6.22852] & sysIIIb = [sysIIIb, 242.791] & psymb = [psymb, !tok.square  ] ;; follow model
  dayob = [dayob, 6.28369] & sysIIIb = [sysIIIb, 279.612] & psymb = [psymb, !tok.square  ] ;; follow model
  dayob = [dayob, 7.38062] & sysIIIb = [sysIIIb, 291.284] & psymb = [psymb, !tok.square  ] ;; first point, follow model
  dayob = [dayob, 7.41333] & sysIIIb = [sysIIIb, 313.198] & psymb = [psymb, !tok.square  ] ;; single point
  dayob = [dayob, 8.23804] & sysIIIb = [sysIIIb, 143.244] & psymb = [psymb, !tok.square  ] ;; decent single-point, deep valley
  dayob = [dayob, 8.26001] & sysIIIb = [sysIIIb, 157.998] & psymb = [psymb, !tok.square  ] ;; possible broad peak
  dayob = [dayob, 8.32642] & sysIIIb = [sysIIIb, 202.100] & psymb = [psymb, !tok.square  ] ;; edge
  dayob = [dayob, 8.43750] & sysIIIb = [sysIIIb, 276.296] & psymb = [psymb, !tok.triangle] ;;
  dayob = [dayob, 9.12061] & sysIIIb = [sysIIIb, 11.9574] & psymb = [psymb, !tok.square  ] ;; isolated 1st point
  dayob = [dayob, 9.19775] & sysIIIb = [sysIIIb, 63.3403] & psymb = [psymb, !tok.square  ] ;; possible peak, bad stats
  dayob = [dayob, 9.21973] & sysIIIb = [sysIIIb, 78.1139] & psymb = [psymb, !tok.square  ] ;; isolated point, maybe peak is here
  dayob = [dayob, 9.29712] & sysIIIb = [sysIIIb, 129.780] & psymb = [psymb, !tok.triangle] ;;
  dayob = [dayob, 9.42041] & sysIIIb = [sysIIIb, 211.974] & psymb = [psymb, !tok.triangle] ;;
  dayob = [dayob, 10.0967] & sysIIIb = [sysIIIb, 302.981] & psymb = [psymb, !tok.square  ] ;; not a sharp blob, follow model
  dayob = [dayob, 10.1843] & sysIIIb = [sysIIIb, 1.47404] & psymb = [psymb, !tok.square  ] ;; two points
  dayob = [dayob, 10.3115] & sysIIIb = [sysIIIb, 86.2841] & psymb = [psymb, !tok.square  ] ;; single point
  dayob = [dayob, 10.4558] & sysIIIb = [sysIIIb, 182.525] & psymb = [psymb, !tok.square  ]

  ;; Unexpected lows
  ;; Carey suggests a little clearer point
  usersym_def = [[-1, 1], $
                 [1, 1], $
                 [0, -1], $
                 [-1, 1]]
  usersym, usersym_def, thick=!p.thick
  dayob = [dayob, 1.37183] & sysIIIb = [sysIIIb, 243.227] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 5.19019] & sysIIIb = [sysIIIb, 270.262] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 5.25317] & sysIIIb = [sysIIIb, 312.303] & psymb = [psymb, !tok.usersym  ]
  ;dayob = [dayob, 5.31885] & sysIIIb = [sysIIIb, 356.122] & psymb = [psymb,!tok.usersym  ] ;; just one point
  dayob = [dayob, 6.25024] & sysIIIb = [sysIIIb, 257.464] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 7.40234] & sysIIIb = [sysIIIb, 305.937] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 8.24902] & sysIIIb = [sysIIIb, 150.546] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 8.33740] & sysIIIb = [sysIIIb, 209.402] & psymb = [psymb, !tok.usersym  ] 
  dayob = [dayob, 8.45947] & sysIIIb = [sysIIIb, 290.930] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 9.40942] & sysIIIb = [sysIIIb, 204.583] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 9.37671] & sysIIIb = [sysIIIb, 182.819] & psymb = [psymb, !tok.usersym  ]
  dayob = [dayob, 10.4448] & sysIIIb = [sysIIIb, 175.114] & psymb = [psymb, !tok.usersym  ]

  ;; Plot points
  ;; psym doesn't take a vector argument
  for ib=0, N_elements(dayob)-1 do begin
     oplot, [dayob[ib]], [sysIIIb[ib]], psym=psymb[ib]
  endfor

  ;; Plot next cycle for select days
  wrap_idx = where(dayob lt 2)
  wrap_idx2 = where((dayob gt 4 and dayob lt 7) $
                   and sysIIIb lt 150)
  wrap_idx = [wrap_idx, wrap_idx2]
  for ib=0, N_elements(wrap_idx)-1 do begin
     oplot, [dayob[wrap_idx[ib]]], [sysIIIb[wrap_idx[ib]]]+360, psym=psymb[wrap_idx[ib]]
  endfor

  ;; Print table in LaTeX format
  UT = nday2date(dayo0 + dayob)
  etype = make_array(N_elements(dayob), value='peak')
  big_idx = where(psymb eq !tok.triangle)
  etype[big_idx] = 'large peak'
  dip_idx = where(psymb eq !tok.psym_x)
  etype[dip_idx] = 'sharp dip'
  ;; Sort in day order
  sidx = sort(dayob)
  for ib=0, N_elements(dayob)-1 do begin
     UTarr = strsplit(UT[sidx[ib]], 'T', /extract)
     print, UTarr[0], ' & ', UTarr[1], ' & ',  sysIIIb[sidx[ib]], ' & ', etype[sidx[ib]], ' \\'
  endfor


  ;;;; Plot usable points (note, no "b" in dayo, these are all of the points)
  ;;wrap_idx = where(dayo lt 2)
  ;;;; Plot both wrapped and unwrapped
  ;;oplot, dayo[wrap_idx], mLONG_3[wrap_idx], psym=!tok.dot, symsize=10
  ;;mLONG_3[wrap_idx] += 360
  ;;wrap_idx = where((dayo gt 4 and dayo lt 7) $
  ;;                 and mLONG_3 lt 150)
  ;;oplot, dayo[wrap_idx], mLONG_3[wrap_idx], psym=!tok.dot, symsize=10
  ;;mLONG_3[wrap_idx] += 360
  ;;oplot, dayo, mLONG_3, psym=!tok.dot, symsize=10

  ;; Plot usable points (note, no "b" in dayo, these are all of the points)
  wrap_idx = where(dayo lt 2)
  ;; Plot both wrapped and unwrapped
  oplot, dayo[wrap_idx], mLONG_3[wrap_idx], psym=!tok.square, symsize=0.10
  mLONG_3[wrap_idx] += 360
  wrap_idx = where((dayo gt 4 and dayo lt 7) $
                   and mLONG_3 lt 150)
  oplot, dayo[wrap_idx], mLONG_3[wrap_idx], psym=!tok.square, symsize=0.10
  mLONG_3[wrap_idx] += 360
  oplot, dayo, mLONG_3, psym=!tok.square, symsize=0.10

  ;;;; Plot peaks of "departure events"
  ;;plot, [0,0], psym=dot, $
  ;;      xrange=[1,11], yrange=[-10,600], $
  ;;      ytickinterval=90, yminor=9, $
  ;;      xstyle=!tok.exact, ystyle=!tok.exact, $
  ;;      xtitle='Day of 1998-10', $
  ;;      ytitle='system III longitude of Io'
  ;;oplot, [1.18384], [117.951], psym=!tok.square ;; single point possibly hight
  ;;oplot, [1.18384], [117.951+360], psym=!tok.square ;; single point possibly hight
  ;;oplot, [1.21680], [139.875], psym=!tok.square ;; single point
  ;;oplot, [1.21680], [139.875+360], psym=!tok.square ;; single point, large excursion
  ;;oplot, [1.33887], [221.32239], psym=!tok.square ;; broad peak 
  ;;oplot, [1.33887], [221.32239+360], psym=!tok.square ;; broad peak 
  ;;;;oplot, [1.36084], [235.83527], psym=!tok.square ;; single point, low statistic
  ;;;;oplot, [1.36084], [235.83527+360], psym=!tok.square
  ;;oplot, [1.39355], [257.74933], psym=!tok.square ;; decent single point statistic
  ;;oplot, [1.39355], [257.74933+360], psym=!tok.square ;; decent single point statistic
  ;;oplot, [1.45996], [302.113], psym=!tok.square ;; broad peak
  ;;oplot, [1.45996], [302.113+360], psym=!tok.square ;; broad peak
  ;;oplot, [5.34082], [10.634], psym=!tok.square ;; two points
  ;;oplot, [5.34082], [10.634+360], psym=!tok.square ;; two points
  ;;oplot, [5.38232], [38.4062], psym=!tok.square ;; single point?
  ;;oplot, [5.38232], [38.4062+360], psym=!tok.square ;; single point?
  ;;oplot, [5.16846], [255.749], psym=!tok.square ;; follow model
  ;;oplot, [5.21216], [284.925], psym=!tok.triangle ;; follow model
  ;;;;oplot, [5.27515], [326.956], psym=!tok.square
  ;;oplot, [5.30786], [348.870], psym=!tok.triangle
  ;;oplot, [6.22852], [242.791], psym=!tok.square ;; follow model
  ;;oplot, [6.28369], [279.612], psym=!tok.square ;; follow model
  ;;oplot, [7.38062], [291.28403], psym=!tok.square ;; first point, follow model
  ;;oplot, [7.41333], [313.198], psym=!tok.square ;; single point
  ;;;;oplot, [7.43530], [327.741], psym=!tok.square ;; too small
  ;;;;oplot, [7.44629], [335.163], psym=!tok.square
  ;;;;oplot, [8.22705], [135.932], psym=!tok.square
  ;;oplot, [8.23804], [143.244], psym=!tok.square ;; decent single-point, deep valley
  ;;oplot, [8.26001], [157.998], psym=!tok.square ;; possible broad peak
  ;;oplot, [8.32642], [202.100], psym=!tok.square ;; edge
  ;;;;oplot, [8.38135], [238.829], psym=!tok.square ;; too small
  ;;;;oplot, [8.40332], [253.423], psym=!tok.square
  ;;oplot, [8.43750], [276.296], psym=!tok.triangle
  ;;oplot, [9.12061], [11.9574], psym=!tok.square ;; isolated 1st point
  ;;oplot, [9.19775], [63.3403], psym=!tok.square ;; possible peak, bad stats
  ;;oplot, [9.21973], [78.113983], psym=!tok.square ;; isolated point, maybe peak is here
  ;;oplot, [9.29712], [129.780], psym=!tok.triangle
;;;;  oplot, [9.33008], [151.684], psym=!tok.square ;; single perturbation
  ;;;;oplot, [9.39844], [197.321], psym=!tok.square ;; possibly just part of larger peak
  ;;oplot, [9.42041], [211.974], psym=!tok.triangle
  ;;oplot, [10.0967], [302.981], psym=!tok.square ;; not a sharp blob, follow model
  ;;;;oplot, [10.1624], [346.820], psym=!tok.square ;; Too low sig
  ;;oplot, [10.1843], [1.47404], psym=!tok.square ;; two points
  ;;;;oplot, [10.2898], [71.751190], psym=!tok.square
  ;;oplot, [10.3115], [86.2841], psym=!tok.square ;; single point
  ;;;;oplot, [10.3335], [100.947], psym=!tok.square
  ;;;;oplot, [10.3560], [115.944], psym=!tok.square
  ;;;;oplot, [10.3789], [131.13318], psym=!tok.square
  ;;;;oplot, [10.4226], [160.319], psym=!tok.square
  ;;oplot, [10.4558], [182.525], psym=!tok.square
  ;;
  ;;;; Unexpected lows
  ;;;;oplot, [1.28247], [183.553], psym=!tok.psym_x
  ;;;;oplot, [1.28247], [183.553+360], psym=!tok.psym_x
  ;;oplot, [1.37183], [243.227], psym=!tok.psym_x
  ;;oplot, [1.37183], [243.227+360], psym=!tok.psym_x
  ;;oplot, [5.19019], [270.262], psym=!tok.psym_x
  ;;oplot, [5.25317], [312.30347], psym=!tok.psym_x
  ;;oplot, [6.25024], [257.464], psym=!tok.psym_x
  ;;oplot, [7.40234], [305.937], psym=!tok.psym_x
  ;;oplot, [8.24902], [150.546], psym=!tok.psym_x
  ;;oplot, [8.33740], [209.402], psym=!tok.psym_x
  ;;;;oplot, [8.41431], [260.755], psym=!tok.psym_x ;; single point
  ;;oplot, [9.40942], [204.583], psym=!tok.psym_x
  ;;oplot, [9.37671], [182.819], psym=!tok.psym_x
  ;;oplot, [10.4448], [175.114], psym=!tok.psym_x
  ;;
  ;;;; Unexpected lows
  ;;;;oplot, [1.28247], [183.553], psym=!tok.psym_x
  ;;;;oplot, [1.28247], [183.553+360], psym=!tok.psym_x
  ;;oplot, [1.37183], [243.227], psym=!tok.psym_x
  ;;oplot, [1.37183], [243.227+360], psym=!tok.psym_x
  ;;oplot, [5.19019], [270.262], psym=!tok.psym_x
  ;;oplot, [5.25317], [312.30347], psym=!tok.psym_x
  ;;oplot, [6.25024], [257.464], psym=!tok.psym_x
  ;;oplot, [7.40234], [305.937], psym=!tok.psym_x
  ;;oplot, [8.24902], [150.546], psym=!tok.psym_x
  ;;oplot, [8.33740], [209.402], psym=!tok.psym_x
  ;;;;oplot, [8.41431], [260.755], psym=!tok.psym_x ;; single point
  ;;oplot, [9.40942], [204.583], psym=!tok.psym_x
  ;;oplot, [9.37671], [182.819], psym=!tok.psym_x
  ;;oplot, [10.4448], [175.114], psym=!tok.psym_x


  ;; Prepare to plot trend lines
  sysIV_III = -600./11 ;; deg per day
  t = findgen(100)/10. + 1

  print, 'Blob slip relative to sys III per day (degrees/day): ', sysIV_III
  print, 'Blob slip relative to sys III per day (cycles/day): ', sysIV_III/360.
  print, 'Blob slip relative to sys III per day (cycles/hr): ', sysIV_III/360./24

  ;; 9 hr 55 min 29.71 
  sysIII = 9. + 55/60. + 29.71/3600.
  print, 'Systen III (hr):', sysIII
  
  print, 'Blib slip (cycles/hr): ', 1./sysIII + sysIV_III/360./24
  print, 'Blib period (hr): ', 1./(1./sysIII + sysIV_III/360./24)
  ;; 10.9 hr, which is a bit long for system IV

  if keyword_set(sysIV) then begin
     ;; See what a more typical sysIV period would look like
     sysIV_III = (1./10.21 - 1./sysIII) *24. *360 ;; deg/day
     print, 'Sys IV 10.2hr drift relative to sysIII (degrees/day): ', sysIV_III
     ;;bl1 = sysIV_III *t + 595
     ;;oplot, t, bl1, linestyle=!tok.dash_3dot
     ;;bl1 = sysIV_III *t + 550
     ;;oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 530
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 500.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 475.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 440.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     ;;bl1 = sysIV_III *t + 425.
     ;;oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 405.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     ;;bl1 = sysIV_III *t + 380.
     ;;oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 360.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 340.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 290.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 250.
     oplot, t, bl1, linestyle=!tok.dash_3dot
  endif

  if keyword_set(phased) then begin
     ;; See what a more typical sysIV period would look like
     sysIV_III = (1./10.667 - 1./sysIII) *24. *360 ;; deg/day
     print, 'Sys IV 10.667hr drift relative to sysIII (degrees/day): ', sysIV_III
     bl1 = sysIV_III *t + 810
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 790
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 750.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 730.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 700.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 660.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 630.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 610.
     oplot, t, bl1, linestyle=!tok.dash_3dot
     bl1 = sysIV_III *t + 570.
     oplot, t, bl1, linestyle=!tok.dash_3dot
  endif

  if keyword_set(sys42) then begin
     ;; See what a more typical sysIV period would look like
     sysIV_III = (1./10.21 - 1./sysIII) *24. *360 ;; deg/day
     ;;bl1 = 2*sysIV_III *t + 730
     ;;oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 690
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 670
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 650
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 610
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 585
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 560
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 540
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 525
     oplot, t, bl1, linestyle=!tok.dashed
     bl1 = 2*sysIV_III *t + 505
     oplot, t, bl1, linestyle=!tok.dashed
     ;;bl1 = 2*sysIV_III *t + 450
     ;;oplot, t, bl1, linestyle=!tok.dashed
  endif

  if keyword_set(zero) then begin
     ;; See what no drift would look like
     sysIV_III = 0
     print, 'Sys IV 10.2hr drift relative to sysIII (degrees/day): ', sysIV_III
     bl1 = sysIV_III *t + 345
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 310.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 280.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 250.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 215.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 180.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 160.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 140.
     oplot, t, bl1, linestyle=!tok.long_dash
     ;;bl1 = sysIV_III *t + 130.
     ;;oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 75.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 40.
     oplot, t, bl1, linestyle=!tok.long_dash
     bl1 = sysIV_III *t + 10.
     oplot, t, bl1, linestyle=!tok.long_dash
  endif

  ;; Had a horrible time trying to use usersym.  Ended up using cgsymcat
  al_legend, ['Peak', 'Large Peak', 'Sharp Dip'], $
             psym=[!tok.square, !tok.triangle, 11], $
             /norm, pos=[0.65, 0.9], box=0
  al_legend, ['sysIII', 'sysIV'], linestyle=[!tok.dashed, !tok.dash_3dot], box=0, /norm, pos=[0.65, 0.75], linsize=0.5

  ;; Work on histogram of differences idea.  Just do peaks for now
  peak_idx = where(psymb ne !tok.psym_x, N_peaks)
  diff_arr = make_array(N_peaks, N_peaks, value=!values.f_NAN)
  for i1=0, N_peaks-2 do begin
     diff_arr[i1, i1+1:N_peaks-1] = dayob[peak_idx[i1+1:N_peaks-1]] - dayob[i1]
  endfor ; i1

  ;;atv, diff_arr

  ;; This wasn't too successful: it just showed me that things
  ;; were taken on different days
  ;;binsize=0.1
  ;;hist = histogram(diff_arr, binsize=0.1, /NAN)
  ;;day_axis = indgen(N_elements(hist)) * binsize
  ;;plot, day_axis, hist

  ;; Try to generate a basic power spectrum using multiples of
  ;; frequencies
  min_freq = 1./max(diff_arr, /NAN)
  max_freq = 1./min(diff_arr, /NAN)
  if NOT keyword_set(N_freq) then $
     N_freq = 1500
  ;; Minimum timing accuracy of lining up peaks is 20 minutes.
  ;; Slighly longer gives a peak near 1/day
  if NOT keyword_set(time_accuracy_minutes) then $
     time_accuracy_minutes = 60.
  time_accuracy_day = time_accuracy_minutes / 60. / 24.
  freq_arr = min_freq + findgen(N_freq)/N_freq * (max_freq - min_freq)
  hit_array = make_array(N_freq, value=0)

  ;; We have a "deadband" or timing accuracy of 20 minutes, maybe
  ;; 40 (see above for assignments) within which it is reasonable
  ;; to assume we have a "hit"
  cycle_accuracy = time_accuracy_day * freq_arr
  ;; As frequency increases, this cycle accuracy erodes the ability of
  ;; the algorithm to separate different distances into different bins
  ;; until at 0.5, all distances match.  A back-of-the-envelope correction,
  ;; which I still don't quite have a good feeling for/about,
  ;; is to simply subtract the line
  bad_idx = where(cycle_accuracy gt 0.5, count)
  if count gt 0 then $
     cycle_accuracy[bad_idx] = 0.5
  junk = where(finite(diff_arr), N_diffs)
  hit_fix = cycle_accuracy*N_diffs*2

  for i_freq=0, N_freq-1 do begin
     ;; Compute number of cycles between each set of two points for
     ;; this frequency.  This will be a real number.  
     num_cycles_arr = diff_arr * freq_arr[i_freq]
     ;; Mark hits where the fractional part of our num_cycles_arr is
     ;; within our deadband 
     hit_idx = where(abs(num_cycles_arr - round(num_cycles_arr)) le cycle_accuracy[i_freq], N_hits)
     ;; I observe a slope that goes up to saturation at the total
     ;; number of features^2.  I assume this is from the effect of our
     ;; deadband, which grows to greater than 0.5 cycles by the time
     ;; we reach high enough frequencies, so everything matches.  So
     ;; we want to take out the matches from 
     ;; we get to ~35/day.  Basically, we are jittering within that deadband
     ;;;; As the sample frequency increases, the effect of our deadband
     ;;;; grows so that finally all points are hits.  Normalize this.
     ;;;; This is ending up not being as easy
     ;;N_hits = N_hits *  (1 - cycle_accuracy) / (1. + 2 * cycle_accuracy) 
     hit_array[i_freq] = N_hits
  endfor ;; each frequency

  ;;plot, freq_arr, hit_array-hit_fix, _EXTRA=extra
  ;;plot, freq_arr, hit_array, _EXTRA=extra

  if keyword_set(ps) then begin
     device,/close
     set_plot, 'x' 
     !P.thick     = oPthick    
     !P.charsize  = oPcharsize 
     !P.charthick = oPcharthick
     !X.thick     = oXthick    
     !Y.thick     = oYthick    
  endif

end

;;      1.18384       117.95065
;;      1.19482       125.21216
;;      1.20581       132.61353
;;      1.21680       139.87494
;;      1.22778       147.11621
;;      1.23853       154.37762
;;      1.24951       161.77905
;;      1.26050       169.03052
;;      1.27148       176.28180
;;      1.28247       183.55322
;;      1.29517       192.16693
;;      1.30615       199.42841
;;      1.31714       206.67969
;;      1.32788       214.07111
;;      1.33887       221.32239
;;      1.34985       228.58386
;;      1.36084       235.83527
;;      1.37183       243.22656
;;      1.38257       250.49805
;;      1.39355       257.74933
;;      1.40527       265.51562
;;      1.41626       272.90698
;;      1.42725       280.17847
;;      1.43823       287.46002
;;      1.44897       294.72144
;;      1.45996       302.11273
;;      1.47095       309.39435
;;      1.48193       316.64575
;;      5.12451       226.57355
;;      5.13550       233.84497
;;      5.14648       241.09625
;;      5.15747       248.34763
;;      5.16846       255.74902
;;      5.17920       263.01041
;;      5.19019       270.26175
;;      5.20117       277.52322
;;      5.21216       284.92462
;;      5.22314       292.17596
;;      5.24243       305.05212
;;      5.25317       312.30347
;;      5.26416       319.70483
;;      5.27515       326.95627
;;      5.28613       334.21765
;;      5.29712       341.47906
;;      5.30786       348.87036
;;      5.31885       356.12170
;;      5.32983       3.3831177
;;      5.34082       10.634399
;;      5.35791       22.095795
;;      5.37158       31.154968
;;      5.38232       38.406250
;;      5.39331       45.817749
;;      5.40845       55.886566
;;      6.21753       235.53961
;;      6.22852       242.79100
;;      6.23926       250.06250
;;      6.25024       257.46390
;;      6.26123       264.70520
;;      6.27222       271.95648
;;      6.28369       279.61203
;;      6.29468       287.01343
;;      6.30566       294.26474
;;      6.44067       24.296753
;;      7.38062       291.28403
;;      7.39136       298.53534
;;      7.40234       305.93674
;;      7.41333       313.19818
;;      7.42432       320.45959
;;      7.43530       327.74109
;;      7.44629       335.16266
;;      7.45703       342.41394
;;      8.22705       135.93230
;;      8.23804       143.24406
;;      8.24902       150.54585
;;      8.26001       157.99762
;;      8.27100       165.32961
;;      8.28198       172.63132
;;      8.29297       179.90282
;;      8.30396       187.15411
;;      8.31494       194.47609
;;      8.32642       202.10017
;;      8.33740       209.40189
;;      8.34839       216.70372
;;      8.35938       224.18565
;;      8.37036       231.49744
;;      8.38135       238.82948
;;      8.39233       246.11099
;;      8.40332       253.42276
;;      8.41431       260.75470
;;      8.42529       268.20660
;;      8.43750       276.29630
;;      8.44849       283.60806
;;      8.45947       290.92993
;;      9.12061       11.957428
;;      9.13159       19.279266
;;      9.14258       26.741150
;;      9.15356       34.032776
;;      9.16455       41.354614
;;      9.17554       48.686554
;;      9.18677       56.018494
;;      9.19775       63.340332
;;      9.20874       70.802216
;;      9.21973       78.113983
;;      9.26416       107.80447
;;      9.27515       115.13638
;;      9.28613       122.46835
;;      9.29712       129.78008
;;      9.30811       137.03143
;;      9.31909       144.43286
;;      9.33008       151.68414
;;      9.34375       160.87430
;;      9.35474       168.12564
;;      9.36572       175.40714
;;      9.37671       182.81863
;;      9.38745       190.06999
;;      9.39844       197.32135
;;      9.40942       204.58278
;;      9.42041       211.97406
;;      9.43140       219.24556
;;      9.44238       226.52713
;;      10.0857       295.68909
;;      10.0967       302.98074
;;      10.1077       310.31265
;;      10.1187       317.57413
;;      10.1296       324.83551
;;      10.1404       332.25708
;;      10.1514       339.51849
;;      10.1624       346.82028
;;      10.1733       354.12201
;;      10.1843       1.4740448
;;      10.1934       7.4577179
;;      10.2065       16.247742
;;      10.2178       23.762115
;;      10.2432       40.808105
;;      10.2546       48.372894
;;      10.2678       57.027908
;;      10.2788       64.499832
;;      10.2898       71.751190
;;      10.3008       79.022736
;;      10.3115       86.284119
;;      10.3225       93.685486
;;      10.3335       100.94696
;;      10.3445       108.19818
;;      10.3560       115.94443
;;      10.3669       123.33572
;;      10.3789       131.13318
;;      10.3899       138.53455
;;      10.4006       145.79605
;;      10.4116       153.04739
;;      10.4226       160.31891
;;      10.4336       167.73038
;;      10.4448       175.11371
;;      10.4558       182.52521
