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
; $Id: ssg_dfort36_plot.pro,v 1.1 2015/02/17 23:02:35 jpmorgen Exp $
;
; $Log: ssg_dfort36_plot.pro,v $
; Revision 1.1  2015/02/17 23:02:35  jpmorgen
; Initial revision
;
;-
pro ssg_dfort36_plot, fname

  if NOT keyword_set(fname) then $
     fname = '/data/io/model/dfort36.15.0267.M0ST2'

  restore, '/data/io/model/dfort36_template.sav'
  d = read_ascii(fname, template=dfort36_template)

  n = 146 ;; number of points per sysIII cycle

  ;; Loop through all chunks in file
  c = 0   ;; chunk number
  done = 0
  repeat begin
     ;; be lazy and use catch to know when we go beyond our array bounds
     CATCH, err
     if err eq 0 then begin
        idx = indgen(n) + c*n
        plot, d.sysIII[idx], d.OI[idx], psym=!tok.plus
        wait, 0.2
        c += 1
     endif else begin
        CATCH, /CANCEL
        done = 1
     endelse
  endrep until done
end
