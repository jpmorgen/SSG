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
; $Id: ssg_model_blip.pro,v 1.1 2015/02/17 23:05:53 jpmorgen Exp $
;
; $Log: ssg_model_blip.pro,v $
; Revision 1.1  2015/02/17 23:05:53  jpmorgen
; Initial revision
;
;-
pro ssg_model_blip, $
   fname, $
   threshold=threshold, $
   binsize=binsize, $
   east=east, $
   west=west

  ;; Default threshold for finding blips
  if N_elements(threshold) eq 0 then $
     threshold = 5


  sysIIIs = 'none'

  if NOT keyword_set(fname) then $
     fname = '/data/io/model/dfort36.15.0267.M0ST2'

  restore, '/data/io/model/dfort36_template.sav'
  d = read_ascii(fname, template=dfort36_template)
  ;; Cast [OI] into kR, which is what the Oliversen et al. database
  ;; uses
  d.OI /= 1000.

  ;; The time spacing is an even ~5 minutes on all points, so just go
  ;; with the diffs.

  N_pts = N_elements(d.sysIII)
  diff1 = d.OI[1:N_pts-1] - d.OI[0:N_pts-2]
  diff2 = diff1[0:N_pts-3] - diff1[1:N_pts-2]
  blip_idx = where(diff2 ge threshold*2, count)

  if count gt 0 then begin
     pfo_array_append, sysIIIs, d.sysIII[blip_idx+1]
  endif


  if N_elements(binsize) eq 0 then $
     binsize = 30.
  yaxis = histogram(sysIIIs, binsize=binsize)

  ;; Make X-axis plot in the middle of the bins
  nbins = 360./binsize
  xaxis = findgen(nbins)*binsize + binsize/2.

  pxaxis = [0, xaxis, 360]
  pyaxis = [yaxis[0], yaxis, yaxis[nbins-1]]
  yrange = [0, max(yaxis)]
  plot, pxaxis, pyaxis, psym=!tok.hist, xstyle=!tok.exact, ystyle=!tok.extend, $
        xrange=[0,360], xtickinterval=90, $
        xtitle='!6System III longitude (degrees)', $
        yrange=yrange, $
        ytitle='Number points', $
        xmargin=[14,0], $
        _EXTRA=extra

end
