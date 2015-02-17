;+

; $Id: ssg_rot_plot.pro,v 1.1 2015/02/17 23:07:56 jpmorgen Exp $

; ssg_rot_plot.pro

; plot cam_rot

;-
pro ssg_rot_plot
  plus = 1
  asterisk = 2
  dot = 3
  diamond = 4
  triangle = 5
  square = 6
  psym_x = 7

  solid=0
  dotted=1
  dashed=2
  dash_dot=3
  dash_3dot = 4
  long_dash=5

  dbopen, 'ssg_reduce', 0
  window,2, xs=1280, ys=512
  dbext,-1,'nday, typecode,sli_cent, m_cam_rot', nday, typecode, sli_cent, m_cam_rot
  for i=0,N_elements(nday)/500 do begin
     chunk_idx = indgen(500) + i*500
     flat_idx = where(typecode[chunk_idx] eq 3)
     obj_idx = where(typecode[chunk_idx] eq 5)
     med = median(m_cam_rot[chunk_idx])
;     yrange = [med - 0.1, med + 0.1]
     yrange = [min(m_cam_rot[chunk_idx],/NAN), max(m_cam_rot[chunk_idx],/NAN)]
;     plot, chunk_idx[flat_idx], m_cam_rot[chunk_idx[flat_idx]], psym=square, $
;           yrange=[med - 0.1, med + 0.1], ystyle=1
;     oplot, chunk_idx[obj_idx], m_cam_rot[chunk_idx[obj_idx]], psym=asterisk
;     oplot, chunk_idx, m_cam_rot[chunk_idx], psym=dot
     plot, nday[chunk_idx[flat_idx]], m_cam_rot[chunk_idx[flat_idx]], $
           psym=square, yrange=yrange, ystyle=1
     oplot, nday[chunk_idx[obj_idx]], m_cam_rot[chunk_idx[obj_idx]], $
            psym=asterisk
     oplot, nday[chunk_idx], m_cam_rot[chunk_idx], psym=dot
     stop
  endfor
end

ssg_rot_plot

end



