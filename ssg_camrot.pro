;; $Id: ssg_camrot.pro,v 1.1 2003/03/10 18:27:26 jpmorgen Exp $

;; ssg_camrot  Perform camera [de]rotation for an ssg image.  This is
;; a little bit of a misnomer, since effects other than the
;; camera, like collimation changes, can contribute to rotation of the
;; slicer pattern.  This would be a simple call to rot, except that
;; the rot (really poly_2D) doesn't do the right thing with
;; missing=!values.f_nan.  Actually, it doesn't do the right thing
;; with missing at all, though that might be because I am tend to have
;; lots and lots of NANs floating around.  At least it does return
;; missing values as 0s, so they are relatively easy to put back to NAN.

function ssg_camrot, in_im, cam_rot, xc, yc, nopivot=nopivot

  im = in_im
  pivot=1
  if keyword_set(no_pivot) then pivot=0

  ;; 0 rotation about a pivot point means do nothing.  Otherwise, we
  ;; might be translating
  if cam_rot eq 0 and keyword_set(pivot) then return, im
  
  ;; Find a number that is not in our image to use a temporary MISSING
  ;; value
;  repeat begin
;     bad = randomu(seed,/double)*(-10000d)
;     bad_idx = where(im eq bad, count)
;  endrep until count eq 0 

  bad=0
  bad_idx = where(im eq bad, count)
  if count ne 0 then message, /CONTINUE, 'WARNING: ' + string(count) + ' pixels=0--these will be replaced with NAN'
  im = rot(im, cam_rot, 1., xc, yc, PIVOT=PIVOT, $
           cubic=-0.5, missing=bad)

  bad_idx = where(im eq bad, count)
  if count gt 0 then begin
     im[bad_idx] = !values.f_nan
  endif

return, im

end
