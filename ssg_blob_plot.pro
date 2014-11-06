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
; $Id: ssg_blob_plot.pro,v 1.3 2014/11/06 02:47:58 jpmorgen Exp $
;
; $Log: ssg_blob_plot.pro,v $
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
pro ssg_blob_plot

  init = {tok_sysvar}
  ;; t = findgen(100)/10.* 3600.*24
  ;; sysIII_ang = t/sysIII*360
  ;; cirrange, sysIII_ang
  ;; print, sysIII_ang

  ;; Plot peaks of "departure events"
  plot, [0,0], psym=dot, $
        xrange=[1,11], yrange=[-10,600], $
        ytickinterval=90, yminor=9, $
        xstyle=!tok.exact, ystyle=!tok.exact, $
        xtitle='Day of 1998-10', $
        ytitle='system III longitude of Io'
  oplot, [1.18384], [117.951], psym=!tok.square
  oplot, [1.18384], [117.951+360], psym=!tok.square
  oplot, [1.21680], [139.875], psym=!tok.square
  oplot, [1.21680], [139.875+360], psym=!tok.square
  oplot, [1.33887], [221.32239], psym=!tok.square
  oplot, [1.33887], [221.32239+360], psym=!tok.square
  oplot, [1.36084], [235.83527], psym=!tok.square
  oplot, [1.36084], [235.83527+360], psym=!tok.square
  oplot, [1.39355], [257.74933], psym=!tok.square
  oplot, [1.39355], [257.74933+360], psym=!tok.square
  oplot, [1.45996], [302.113], psym=!tok.square
  oplot, [1.45996], [302.113+360], psym=!tok.square
  oplot, [5.34082], [10.634+360], psym=!tok.square
  oplot, [5.34082], [10.634], psym=!tok.square
  oplot, [5.38232], [38.4062+360], psym=!tok.square
  oplot, [5.38232], [38.4062], psym=!tok.square
  oplot, [5.16846], [255.749], psym=!tok.square ;; follow model
  oplot, [5.21216], [284.925], psym=!tok.asterisk ;; follow model
  ;;oplot, [5.27515], [326.956], psym=!tok.square
  oplot, [5.30786], [348.870], psym=!tok.asterisk
  oplot, [6.22852], [242.791], psym=!tok.square ;; follow model
  oplot, [6.28369], [279.612], psym=!tok.square ;; follow model
  oplot, [7.38062], [291.28403], psym=!tok.square ;; first point, follow model
  oplot, [7.41333], [313.198], psym=!tok.square
  oplot, [7.43530], [327.741], psym=!tok.square
  ;;oplot, [7.44629], [335.163], psym=!tok.square
  ;;oplot, [8.22705], [135.932], psym=!tok.square
  oplot, [8.23804], [143.244], psym=!tok.square
  oplot, [8.26001], [157.998], psym=!tok.square
  oplot, [8.32642], [202.100], psym=!tok.square
  oplot, [8.38135], [238.829], psym=!tok.square
  ;;oplot, [8.40332], [253.423], psym=!tok.square
  oplot, [8.43750], [276.296], psym=!tok.asterisk
  oplot, [9.12061], [11.9574], psym=!tok.square
  oplot, [9.19775], [63.3403], psym=!tok.square
  oplot, [9.21973], [78.113983], psym=!tok.square
  oplot, [9.29712], [129.780], psym=!tok.asterisk
;;  oplot, [9.33008], [151.684], psym=!tok.square
  oplot, [9.39844], [197.321], psym=!tok.square
  oplot, [9.42041], [211.974], psym=!tok.asterisk
  oplot, [10.0967], [302.981], psym=!tok.square
  oplot, [10.1624], [346.820], psym=!tok.square
  oplot, [10.1843], [1.47404], psym=!tok.asterisk
  oplot, [10.2898], [71.751190], psym=!tok.square
  oplot, [10.3115], [86.2841], psym=!tok.asterisk
  oplot, [10.3335], [100.947], psym=!tok.square
  oplot, [10.3560], [115.944], psym=!tok.square
  oplot, [10.3789], [131.13318], psym=!tok.square
  oplot, [10.4226], [160.319], psym=!tok.square
  oplot, [10.4558], [182.525], psym=!tok.square

  ;; Unexpected lows
  oplot, [1.28247], [183.553], psym=!tok.psym_x
  oplot, [1.28247], [183.553+360], psym=!tok.psym_x
  oplot, [1.37183], [243.227], psym=!tok.psym_x
  oplot, [1.37183], [243.227+360], psym=!tok.psym_x
  oplot, [5.19019], [270.262], psym=!tok.psym_x
  oplot, [5.25317], [312.30347], psym=!tok.psym_x
  oplot, [6.25024], [257.464], psym=!tok.psym_x
  oplot, [7.40234], [305.937], psym=!tok.psym_x
  oplot, [8.24902], [150.546], psym=!tok.psym_x
  oplot, [8.33740], [209.402], psym=!tok.psym_x
  oplot, [8.41431], [260.755], psym=!tok.psym_x
  oplot, [9.40942], [204.583], psym=!tok.psym_x
  oplot, [9.37671], [182.819], psym=!tok.psym_x
  oplot, [10.4448], [175.114], psym=!tok.psym_x

  ;; Plot usable points
  dbopen,'io6300_integrated'
  entries = dbfind("obj_code=1", dbfind("intensity>0.05", $
                   dbfind("nday>3195", dbfind("nday<3220"))))
  dbext, entries, 'nday, LONG_3, intensity, err_intensity', $
         mndays, mLONG_3, mintensities, merr_intensities
  dbclose
  dayo = mndays-3194
  wrap_idx = where(dayo lt 2)
  ;; Plot both wrapped and unwrapped
  oplot, dayo[wrap_idx], mLONG_3[wrap_idx], psym=!tok.dot
  mLONG_3[wrap_idx] += 360
  wrap_idx = where((dayo gt 4 and dayo lt 7) $
                   and mLONG_3 lt 150)
  oplot, dayo[wrap_idx], mLONG_3[wrap_idx], psym=!tok.dot
  mLONG_3[wrap_idx] += 360
  oplot, dayo, mLONG_3, psym=!tok.dot

  ;; Plot trend lines
  sysIV_III = -600./11 ;; deg per day
  t = findgen(100)/10. + 1
;;  bl1 = sysIV_III *t + 640.
;;  oplot, t, bl1, linestyle=!tok.dashed
;;  bl2 = sysIV_III *t + 735.
;;  oplot, t, bl2, linestyle=!tok.dashed
;;  bl3 = sysIV_III *t + 560.
;;  oplot, t, bl3, linestyle=!tok.dashed


  print, 'Blob slip relative to sys III per day (degrees/day): ', sysIV_III
  print, 'Blob slip relative to sys III per day (cycles/day): ', sysIV_III/360.
  print, 'Blob slip relative to sys III per day (cycles/hr): ', sysIV_III/360./24

  ;; 9 hr 55 min 29.71 
  sysIII = 9. + 55/60. + 29.71/3600.
  print, 'Systen III (hr):', sysIII
  
  print, 'Blib slip (cycles/hr): ', 1./sysIII + sysIV_III/360./24
  print, 'Blib period (hr): ', 1./(1./sysIII + sysIV_III/360./24)
  ;; 10.9 hr, which is a bit long for system IV

  ;; See what a more typical sysIV period would look like
  sysIV_III = (1./10.21 - 1./sysIII) *24. *360 ;; deg/day
  print, 'Sys IV 10.2hr drift relative to sysIII (degrees/day): ', sysIV_III
  bl1 = sysIV_III *t + 595
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 550
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 530
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 500.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 480.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 440.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 425.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 405.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 380.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 360.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 340.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 290.
  oplot, t, bl1, linestyle=!tok.dash_3dot
  bl1 = sysIV_III *t + 250.
  oplot, t, bl1, linestyle=!tok.dash_3dot

  ;;;; See what no drift would look like
  ;;sysIV_III = 0
  ;;print, 'Sys IV 10.2hr drift relative to sysIII (degrees/day): ', sysIV_III
  ;;bl1 = sysIV_III *t + 345
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 310.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 280.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 250.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 215.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 180.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 160.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 140.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 130.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 75.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 40.
  ;;oplot, t, bl1, linestyle=!tok.long_dash
  ;;bl1 = sysIV_III *t + 10.
  ;;oplot, t, bl1, linestyle=!tok.long_dash




 ;;  window, 1
 ;;
 ;;  o4_idx = where(floor(dayo) eq 4)
 ;;  plot, mLONG_3[o4_idx], mintensities[o4_idx]
 ;;  oploterr, mLONG_3[o4_idx], mintensities[o4_idx], merr_intensities[o4_idx]


  ;;
  ;;  1998-10-1	& 		& 140?, 210? 305?\\
  ;;1998-10-5	& 		& 345\\
  ;;1998-10-6	& 		& 285 \\
  ;;1998-10-7	& 		& - \\
  ;;1998-10-8	& 		& 140, 235?, 285\\
  ;;1998-10-9	& 		& 130, 205\\
  ;;1998-10-10	& 		& 0 \\

  
  cirrange, mlong_3
  for i=0,N_elements(dayo)-1 do print, dayo[i], mlong_3[i]

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
