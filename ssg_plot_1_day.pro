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
; $Id: ssg_plot_1_day.pro,v 1.1 2015/02/17 23:07:11 jpmorgen Exp $
;
; $Log: ssg_plot_1_day.pro,v $
; Revision 1.1  2015/02/17 23:07:11  jpmorgen
; Initial revision
;
;-
pro ssg_plot_1_day, nday, ps=ps, _EXTRA=extra

  init = {ssg_sysvar}
  init = {tok_sysvar}

  if N_elements(nday) eq 0 then $
     message, 'ERROR: nday required.  Use print, floor(ssg_select()) to get an nday'

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
       ps = 'ssg_plot_1_day_out.eps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /encap
  endif

  ;; Extract stuff from Melanie's database

  dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'
  entries=dbfind("err_intensity<10", $
                 dbfind("intensity>0.001", $
                        dbfind("lambda=6300", $
                               dbfind("obj_code=1", $
                                      dbfind(string("nday>", nday), $
                                             dbfind(string("nday<", nday+1)))))))

  dbext, entries, 'date, nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mdate, mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc
  dbext, entries, 'nrows, numlines, deldot, deldot_m, err_deldot_m', mnrows, mnumlines, mdeldot, mdeldot_m, merr_deldot_m
  dbext, entries, 'phi', mphi

  dbclose

  plot, mlong_3, mintensity, psym=!tok.square, $
        xrange=[0,360], yrange=[0, 15], $
        xtickinterval=90, xminor=9, $
        xtitle='!6Io System III Longitude (deg)', $
        ytitle= '[O I] 6300 !6!sA!r!u!9 %!6!n Brightness (kR)', $
        _EXTRA=extra
  
  oploterr, mlong_3, mintensity, merr_intensity, !tok.dot

  adbname = 'io_oi_analyze'
  dbopen, adbname, 0
  aentries = dbfind("obj_code=1", $
                    dbfind("redchisq<5", $
                           dbfind(string("nday>", nday), $
                                  dbfind(string("nday<", nday+1)))))
  dbext, aentries, "nday, long_3, phi, intensity, err_intensity, alf, delta, wc, err_wc", andays, along_3s, aphis, aintensities, aerr_intensities, aalfs, adeltas, awcs, aerr_wcs
  
  dbclose
  oplot, along_3s, aintensities, psym=!tok.diamond
  oploterr, along_3s, aintensities, aerr_intensities, !tok.dot  

  ;;arrow, 284.925, 1, 284.925, 5, /data
  ;;arrow, 341.479, 1, 341.479, 5, /data
  ;;arrow, 255.749, 1, 255.749, 4, /data
  ;;
  ;;arrow, 270.262, 14, 270.262, 10, /data
  ;;arrow, 312.303, 14, 312.303, 10, /data
  
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
