;+
; NAME: ssg_sysiv_search
;
; PURPOSE: Search system IV in the Io [OI] signal
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
; $Id: ssg_sysiv_search.pro,v 1.2 2013/07/18 17:08:47 jpmorgen Exp $
;
; $Log: ssg_sysiv_search.pro,v $
; Revision 1.2  2013/07/18 17:08:47  jpmorgen
; Basic version.  Hope units of PSD are correct on plot
;
; Revision 1.1  2013/07/18 16:41:23  jpmorgen
; Initial revision
;
; Revision 1.6  2013/04/29 16:39:47  jpmorgen
; About to add negative blips
;
; Revision 1.5  2013/01/31 13:39:20  jpmorgen
; About to add time limit
;
; Revision 1.4  2012/11/27 21:57:56  jpmorgen
; Fixed a bunch of stuff.  About to switch to portrait PS output
;
; Revision 1.3  2012/11/26 22:36:29  jpmorgen
; Version with bug fixed
;
; Revision 1.2  2012/11/26 22:36:10  jpmorgen
; Version I submitted for abstract
;
; Revision 1.1  2012/07/20 01:44:31  jpmorgen
; Initial revision
;
;-
pro ssg_sysiv_search, $
   ps=ps, $
   jpeg=jpeg, $
   _EXTRA=extra ; args to plot

  init = {tok_sysvar}

  ;; Be polite, but get the line thicknesses we need for PS output
  ;; (leave for terminal output since that gets us started on PS output)
  oPthick     = !P.thick
  oPcharsize  = !P.charsize
  oPcharthick = !P.charthick
  oXthick     = !X.thick
  oYthick     = !Y.thick


  ;; Initialize postscipt output
  if keyword_set(ps) then begin
     !P.thick = 3
      !P.charsize = 1.5
      !P.charthick = 2
      !X.thick = 2
      !Y.thick = 2      
      if size(ps, /TNAME) ne 'STRING' then $
        ps = 'ssg_sysiv_search_out.eps'
      set_plot, 'ps'
      device, /portrait, filename=ps, /encap
   endif

   dbclose ;; just in case
   dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'

   ;; Find all the Io [OI] measurements
   Io_OI = dbfind(/silent, "intensity>0.001", $             ;; screen out junk
                  dbfind(/silent, "lambda=6300", $          ;; make sure we have the right line
                         dbfind(/silent, "obj_code=1")))    ;; Io

   print, 'Total number of Io [OI] points: ', N_elements(Io_OI)

   dbext, Io_OI, 'nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc

   dbclose

   ;; scargle input is time, signal, output is power spectral density
   ;; as a function of angular frequency, omega.  Convert mnday to
   ;; hours, so omega, reads in 1/hour

   scargle, mnday/24., mintensity, omega, psd

   ;; plot frequency, rather than angular frequency.
   plot, omega/(2.*!pi), psd*(2.*!pi), $
         xtitle='Frequency (hr!u-1!n)', $
         ytitle='Power Spectral density'

     
  if keyword_set(ps) then begin
     device,/close
     set_plot, 'x' 
  endif

  if keyword_set(jpeg) then begin
     if size(jpeg, /TNAME) ne 'STRING' then $
       jpeg = 'ssg_sysiv_search_out.jpeg'
     write_jpeg, jpeg, tvrd(true=1), quality=75, true=1
  endif

  !P.thick     = oPthick    
  !P.charsize  = oPcharsize 
  !P.charthick = oPcharthick
  !X.thick     = oXthick    
  !Y.thick     = oYthick    


end
