; $Id: ssg_edge_find.pro,v 1.2 2003/06/11 19:48:56 jpmorgen Exp $

; ssg_edge_find.pro a version of edge_find that is optomized to find
; the edge in a the SSG cross-dispersion spectrum

function ssg_edge_find, yin, side, threshold=threshold, contrast=contrast, limits=in_limits, yerr=yerr, error=error, plot=plot

  y = yin
  npts = N_elements(y)
  side = strlowcase(side)
  if side eq 'right' then begin
     y = reverse(y)
     if keyword_set(yerr) then $
       yerr = reverse(yerr)
     if keyword_set(in_limits) then $
       limits = [npts-1-in_limits[1], npts-1-in_limits[0]]
     edge = ssg_edge_find(y, 'left', contrast=contrast, limits=limits, yerr=yerr, error=error, plot=plot)
     edge = npts-1 - edge
     return, edge
  endif else $
    if side ne 'left' then $
    message, 'ERROR: specify whether you are looking for an edge from the ''left'' or ''right'' side of the array'

  if keyword_set(in_limits) then $
    limits = in_limits $
  else $
    limits=[0,npts-1,0,npts-1]

  ;; What I want is the first maximum in second derivative.  Do the
  ;; calculations on the whole cross-dispersion spectrum, otherwise I
  ;; get edge effects at the beginning and end of the array I am
  ;; working with
  dy = deriv(yin)
  d2y = deriv(dy)
  ;neg_idx = where(dy lt 0, count) 
  ;if count gt 0 then $
  ;  d2y[neg_idx] = -d2y[neg_idx]
  ;; But the contrast is pretty weak compared to the peaks, so divide
  ;; by the y value and slope
  fun = d2y/y
  ;; Now take a subset of the function which should be centered on the
  ;; feature, if limits are set properly in the calling program
  fun = fun[limits[0]:limits[1]]

  ;; I am not sure how to handle errors eith derivatives.  Let's at
  ;; least do it proportionally
  if keyword_set(yerr) then begin
     err = deriv(deriv(yerr))/y
     err = err[limits[0]:limits[1]]
     return, first_peak_find(fun, 'left', threshold=threshold, $
                             contrast=contrast, $
                             yerr=err, error=error, plot=plot) $
             + limits[0]
  endif
  return, first_peak_find(fun, 'left', threshold=threshold, $
                          contrast=contrast, error=error, plot=plot) $
          + limits[0]
end
