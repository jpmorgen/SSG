; +
; $Id: ssg_dg_struct__define.pro,v 1.1 2015/02/17 23:03:02 jpmorgen Exp $

; ssg_dg_struct__define.pro 

; This procedure makes use of the handy feature in IDL 5 that calls
; the procedure mystruct__define when mystruct is referenced.
; Unfortunately, if IDL calls this proceedure itself, it uses its own
; idea of what null values should be.  Call explicitly with an
; if you need to have a default structure with different initial values.

;; Add an ssg_struct onto sso_dg_struct to make sure we don't have
;; collisions in the !sso.dgs list.


pro ssg_dg_struct__define, dg_struct=dg_struct

  ssg_struct__define, ssg_struct=ssg_struct
  sso_dg_struct__define, dg_struct=sso_dg_struct
  ssg = {ssg : ssg_struct}
  dg_struct = struct_append(sso_dg_struct, ssg, name="ssg_dg_struct")
end


