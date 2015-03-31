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
; $Id:$
;
; $Log:$
;-
pro ssg_blob_phase, $
   ndays=ndays, $
   long_3s=long_3s, $
   phis=phis, $
   target_wl_range=target_wl_range, $
   wls=wls

  init = {ssg_sysvar}

  if N_elements(target_wl_range) eq 0 then $
     target_wl_range = 2*!dpi/[8,12] ;; include system III and system IV periods with ample room

  ;; --> While using Melanie's database, I need to do these
  ;; calculations here, which won't be as accurate as the ones
  ;; from JPL
  
  ;; Compute time in the frame of Jupiter.
  jJDs = ndays+!ssg.JDnday0 ;; Julian days uncorrected for light travel time
  ;; Julian day of each blob in Jupiter's frame
  jJDs -= jupinfo(jJDs, body='Jupiter', quantity='distance') / !ssg.c
  
  wls = 'None'
  for iblob1=0,N_elements(jJDs)-1 do begin
     deltats = jJDs[iblob1] - jJDs
     ;; Make sure we only collect omega_lambdas from positive deltas
     for iblob2=iblob1+1,N_elements(jJDs)-1 do begin
        ;; Calculate the range of integers represented by our target
        ;; omega range so we get all of the relevant omegas
        integer_range = (2*!dpi/!ssg.TIo + target_wl_range) * deltats[iblob2] / (2 * !dpi)
        integer_range = round(integer_range)
        N_integers = abs(integer_range[1] - integer_range[0]) + 1
        integers = indgen(N_integers) + integer_range[0]
        wl = 2*!dpi * integers / deltats[iblob2] - 2*!dpi/!ssg.TIo
        ;; Collect omega_lambdas 
        pfo_array_append, wls, wl
     endfor ;; each delta t     
  endfor ;; each blob
end
