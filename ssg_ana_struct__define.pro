; ssg_ana_struct__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; Make a parinfo structure that has everything that we need to keep
;; track of ssg fit information and analysis.  SSO is necessary for
;; the fitting routine and SSG has some tokens for nday and fit
;; version so that the results from each fit can be stored easily.

;; Mon Jul 27 12:50:26 2015  jpmorgen@byted
;; Append an sso_ana_struct onto an ssg_parinfo for analysis of close lines

pro ssg_ana_struct__define, parinfo=parinfo
  ssg_parinfo__define, parinfo=ssg_parinfo
  sso_ana_struct__define, sso_ana_struct=sso_ana_struct
  sso_ana = {sso_ana : sso_ana_struct}
  parinfo = struct_append(ssg_parinfo, sso_ana, name="ssg_ana_parinfo")
end
