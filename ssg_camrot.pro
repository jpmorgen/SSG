;; $Id: ssg_camrot.pro,v 1.2 2015/03/04 15:58:42 jpmorgen Exp $

;; ssg_camrot  Perform camera [de]rotation for an ssg image.  This is
;; a little bit of a misnomer, since effects other than the
;; camera, like collimation changes, can contribute to rotation of the
;; slicer pattern.  This would be a simple call to rot, except that
;; the rot (really poly_2D) doesn't do the right thing with
;; missing=!values.f_nan.  Actually, it doesn't do the right thing
;; with missing at all, though that might be because I am tend to have
;; lots and lots of NANs floating around.  At least it does return
;; missing values as 0s, so they are relatively easy to put back to NAN.

function ssg_camrot, in_im, cam_rot, xc, yc, nopivot=nopivot, quiet=quiet, grow_mask=grow_mask

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

  ;; Replace bad columns with estimates so that rot won't make
  ;; NAN areas grow unnecessarily
  im = ssg_column_replace(im, mask, nbad, grow_mask=grow_mask)

  im = rot(im, cam_rot, 1., xc, yc, PIVOT=PIVOT, $
           cubic=-0.5, missing=!values.f_nan)

  ;; Re-apply the bad column mask on the way out
  return, im+mask

end
