; +
; $Id: ssg_struct__define.pro,v 1.1 2015/02/17 23:08:38 jpmorgen Exp $

; ssg_struct__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; SSG specific tags to be added to the parinfo structure.  The idea
;; is to make a set of tags that allow a unique identity for each fit
;; version of each parameter.  I could duplicate lc and sso structures
;; underneath this, but until I really need to do that, might as well
;; just rely on them in place (see ssg_parinfo).  I might want to
;; expand fver to a multi-tag field when I am playing with automatic
;; fitting (e.g. a tag for each class of fit I try or something.

;; Tag		Meaning
;; nday		nday of observation
;; fver		fit version


pro ssg_struct__define, ssg_struct=ssg_struct
  ssg_struct $
    = {ssg_struct, $
       nday		: 0D, $
       fver		: 0}
  ssg_struct.nday = !values.d_nan
  ssg_struct.fver = -1 ;; Version not initialized
  
end
