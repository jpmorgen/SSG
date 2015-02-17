; +
; $Id: ssg_parinfo__define.pro,v 1.1 2015/02/17 23:06:51 jpmorgen Exp $

; ssg_parinfo__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; Make a parinfo structure that has everything that we need to keep
;; track of ssg fit information.  SSO is necessary for the fitting
;; routine, LC keeps track of the source catalog from which the line
;; came, and SSG has some tokens for nday and fit version so that the
;; results from each fit can be stored easily.

;; Mon Apr 19 19:29:31 2004  jpmorgen
;; Nuke lc for now, since I use lc2sso to put catalog information into
;; an sso parinfo

pro ssg_parinfo__define, parinfo=parinfo
  sso_parinfo__define, parinfo=sso_parinfo
;  lc_struct = {lc_struct}
  ssg_struct__define, ssg_struct=ssg_struct
;  lc = {lc : lc_struct}
  ssg = {ssg : ssg_struct}
;  temp_parinfo = struct_append(sso_parinfo, lc, name="ssg_temp_parinfo")
;  parinfo = struct_append(temp_parinfo, ssg, name="ssg_parinfo")
  parinfo = struct_append(sso_parinfo, ssg, name="ssg_parinfo")

end
