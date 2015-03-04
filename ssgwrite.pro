; $Id: ssgwrite.pro,v 1.2 2015/03/04 15:53:04 jpmorgen Exp $

; ssgwrite writes the primary data array and the statistical error
; extension.  It should be used in preference to writefits everywhere

pro ssgwrite, fname, im, hdr, eim, ehdr, TV=tv, REUSE=reuse, zoom=zoom, VERBOSE=verbose

;  ON_ERROR, 2
  silent = 1
  if keyword_set(verbose) then silent = 0

  ;; Code from writefits so that I have control over verbosity
  ;; (no silent keyword for writefits)
  check_fits, im, hdr, /UPDATE, /FITS, SILENT=silent

  sxaddhist, string('(ssgwrite.pro) ', systime(/UTC), ' UT'), hdr
  sxaddhist, string('(ssgwrite.pro) wrote as file ', fname), hdr
  writefits, fname, im, hdr
  writefits, fname, eim, ehdr, /APPEND
  ;; Quietly change the mode and group of the file to be group lyra
  ;; writable so other people can work on this
  spawn, string('chmod g+w ', fname), txtout, errout
  spawn, string('chgrp lyra ', fname), txtout, errout

  if keyword_set(tv) then begin
     display, im, title=filename, REUSE=reuse, zoom=zoom, rotate=rotate
  endif

  return

end

