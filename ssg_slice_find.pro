; $Id: ssg_slice_find.pro,v 1.1 2015/02/17 23:08:15 jpmorgen Exp $

; ssg_slice_find.pro find the positions of slice boundaries by looking
; for local minima in an ssg cross dispersion spectrum

function ssg_slice_find, yin, side, threshold=threshold, contrast=contrast, limits=in_limits, yerr=yerr, error=error, plot=plot

  y = yin
  npts = N_elements(y)

  ;; What I want is the first maximum in second derivative.  Do the
  ;; calculations on the whole cross-dispersion spectrum, otherwise I
  ;; get edge effects at the beginning and end of the array I am
  ;; working with
  dy = deriv(yin)
  d2y = deriv(dy)
  plot, d2y
  return, 0
end
