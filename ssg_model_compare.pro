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
; $Id: ssg_model_compare.pro,v 1.1 2015/02/17 23:06:14 jpmorgen Exp $
;
; $Log: ssg_model_compare.pro,v $
; Revision 1.1  2015/02/17 23:06:14  jpmorgen
; Initial revision
;
;-
pro ssg_model_compare

  init = {ssg_sysvar}
  init = {tok_sysvar}

  ;; Extract stuff from Melanie's database

  dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'
  entries=dbfind("err_intensity<10", $
                 dbfind("intensity>0.001", $
                        dbfind("lambda=6300", $
                               dbfind("obj_code=1"))))

  dbext, entries, 'date, nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mdate, mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc
  dbext, entries, 'nrows, numlines, deldot, deldot_m, err_deldot_m', mnrows, mnumlines, mdeldot, mdeldot_m, merr_deldot_m
  dbext, entries, 'phi', mphi

  dbclose

  ;; We want to see what the continuum looks like on each day, both
  ;; with average and median.  Max reports things with calendar dates.
  ;; Do that conversion

  ;; I created the scalefacs_template using:
  ;; scalefacs_template = ascii_template('/data/io/ssg/analysis/max/scalefacs.dat')
  ;; save, scalefacs_template, filename='/data/io/ssg/analysis/max/scalefacs.sav'

  restore, '/data/io/ssg/analysis/max/scalefacs.sav'
  sf = read_ascii('/data/io/ssg/analysis/max/scalefacs.dat', template=scalefacs_template)

  ;; --> FIX 6 .  Have to stop reading early because of text at the
  ;; bottom.  Can't write files right now because of license problem
  nmodeled = N_elements(sf.year) - 6
  sf_comp = {nday : 0., $
             scale : 0., $
             av_intensity : 0., $
             med_intensity : 0., $
             av_err_intensity : 0., $
             med_err_intensity : 0., $
             av_fcont : 0., $
             med_fcont : 0., $
             av_err_fcont : 0., $
             med_err_fcont : 0., $
             av_wc : 0., $
             med_wc : 0., $
             av_err_wc : 0., $
             med_err_wc : 0.}
  ;; Loop through each day max modeled

  for im=0,nmodeled-1 do begin

     ;; juldate only works one date at a time.
     datearr = [sf.year[im], sf.month[im], sf.day[im]]
     juldate, double(datearr), rawjd
     nday = rawjd + !eph.jd_reduced - !ssg.JDnday0
     sf_comp.nday = nday
     sf_comp.scale = sf.scale[im]

     ;; Compare to Melanie's fits
     midx = where(round(nday) eq floor(mnday), count)
     if count eq 0 then $
        message, 'ERROR: didn''t get nday match'
     sf_comp.av_intensity = mean(mintensity[midx])
     sf_comp.med_intensity = median(mintensity[midx])
     sf_comp.av_fcont = mean(mfcont[midx])
     sf_comp.med_fcont = median(mfcont[midx])
     sf_comp.av_wc = mean(mwc[midx])
     sf_comp.med_wc = median(mwc[midx])
     sf_comp.av_err_fcont = mean(merr_fcont[midx])
     sf_comp.med_err_fcont = median(merr_fcont[midx])
     sf_comp.av_err_wc = mean(merr_wc[midx])
     sf_comp.med_err_wc = median(merr_wc[midx])

     sf_comp_arr = array_append(sf_comp, sf_comp_arr)
  endfor ;; each day max modeled

  window, 0
  plot, sf_comp_arr.av_fcont, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Average continuum', ytitle='model scale factor'

  window,1
  plot, sf_comp_arr.med_fcont, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Median continuum', ytitle='model scale factor'

  window, 2
  plot, sf_comp_arr.av_wc, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Average Line Width', ytitle='model scale factor'

  window,3
  plot, sf_comp_arr.med_wc, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Median Line Width', ytitle='model scale factor'


  window, 4
  plot, sf_comp_arr.av_err_fcont, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Average continuum error', ytitle='model scale factor', $
        xrange=[0,0.3]

  window,5
  plot, sf_comp_arr.med_err_fcont, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Median continuum error', ytitle='model scale factor', $
        xrange=[0,0.3]

  window, 6
  plot, sf_comp_arr.av_err_wc, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Average Line Width error', ytitle='model scale factor'

  window,7
  plot, sf_comp_arr.med_err_wc, sf_comp_arr.scale, psym=!tok.plus, $
        xtitle='Median Line Width error', ytitle='model scale factor'

  window, 8
  plot, sf_comp_arr.av_intensity, sf_comp_arr.av_fcont, psym=!tok.plus, $
        xtitle='Average Intensity', ytitle='Average continuum'

  window,9
  plot, sf_comp_arr.med_intensity, sf_comp_arr.med_fcont, psym=!tok.plus, $
        xtitle='Median Intensity', ytitle='Median continuum'


  

end

