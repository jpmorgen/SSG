;+
; $Id: ssg_fit_camrot.pro,v 1.2 2002/11/14 19:17:27 jpmorgen Exp $

; ssg_fit_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_fit_camrot, indir, VERBOSE=verbose, order=order, sigma_cut=sigma_cut, $
                    width=width, noninteractive=noninteractive, write=write

;  ON_ERROR, 2
  cd, indir
  if N_elements(order) eq 0 then order=0
  if NOT keyword_set(sigma_cut) then sigma_cut=5
  if NOT keyword_set(width) then width=3

  silent = 1
  if keyword_set(verbose) then silent = 0

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


  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind(string("dir=", indir))
  dbext, entries, "fname, nday, date, m_cam_rot", files, ndays, dates, m_cam_rot
  angles = m_cam_rot
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(ndays)     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)

  filtered_angles = angles
  if width gt 1 and width lt N_elements(angles) then $
    filtered_angles = median(angles, width)
  flat_angles = angles - filtered_angles
  stdev = stddev(filtered_angles, /NAN)
  sigmas = flat_angles/stdev
  badidx = where(abs(sigmas) gt sigma_cut, count)
  if count gt 0 then angles[badidx] = !values.f_nan

  refit=1
  if keyword_set(noninteractive) then refit = 0
  window,7
  repeat begin
     good_idx = where(finite(angles), count)
     if count eq 0 then message, 'ERROR: no good camera rotation measurements found'
     coefs = poly_fit(ndays-this_nday[good_idx], angles[good_idx], order)
     
     cam_rot = fltarr(nf)
     for ci=0,order do begin
        cam_rot = cam_rot + coefs[ci]*(ndays-this_nday)^ci
     endfor

     plot, ndays, angles, $
           title=string('Camera rotation angles in ', indir), $
           xtickunits='Hours', $
           yrange=[min([angles,angles], /NAN), $
                   max([angles, angles], /NAN)], $
           xstyle=2, ystyle=2, psym=plus, $
           xtitle=string('UT time (Hours) ', utdate), $
           ytitle='Angle (degrees)'
     oplot, ndays, cam_rot
     ;;oploterr, ndays, angles, sigmas

     if NOT keyword_set(noninteractive) then begin
        ;; User selects a bad point
        message, /CONTINUE, 'Select bad points with leftmost mouse button, rightmost button exits'
        cursor, badnday, badval, /DOWN, /DATA
        if !MOUSE.button eq 1 then begin
           dxs = ndays[good_idx] - badnday
           dys = angles[good_idx] - badval
           dists = dxs*dxs + dys*dys
           junk = min(dists, bad_idx, /NAN)
           angles[good_idx[bad_idx]] = !values.f_nan
        endif ;; leftmost mouse button
        if !MOUSE.button eq 2 then begin
           message, /CONTINUE, 'HEY, I said left or right buttons, not middle!'
        endif
        if !MOUSE.button eq 4 then begin
           message, /CONTINUE, 'DONE'
           refit = 0
        endif
     endif
     
  endrep until refit eq  0


  if NOT keyword_set(noninteractive) then begin
     if NOT keyword_set(write) then $
       repeat begin
        message, /CONTINUE, 'Write this fit to the database and FITS headers?([Y]/N)'
        answer = get_kbrd(1)
        if byte(answer) eq 10 then answer = 'Y'
        answer = strupcase(answer)
     endrep until answer eq 'Y' or answer eq 'N'
     if answer eq 'Y' then write=1
  endif

  ;; Now write the information to the FITS headers
  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'cam_rot', cam_rot
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated camera rotation stuff in ' + dbname

     err=0
     message, /INFORMATIONAL, 'Writing values in FITS headers'
     for i=0,nf-1 do begin
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           im = readfits(files[i], hdr, silent=silent) ; Just reading fits header
           sxaddhist, string('(ssg_fit_camrot.pro) ', systime(/UTC), ' UT'), hdr
           sxaddhist, string('(ssg_fit_camrot.pro) sigma_cut= ', sigma_cut), hdr
           sxaddpar, hdr, 'M_CAM_ROT', m_cam_rot[i], 'Measured camera rotation (clockwise)'
           sxaddpar, hdr, 'CAM_ROT', cam_rot[i], 'Predicted camera rotation (clockwise)'
           for oi=0,order do begin
              sxaddpar, hdr, string(format='("CAM_RP", i1)', oi), coefs[oi], 'poly. coef. in nday, origin= fix(nday,0)'
           endfor
           writefits, files[i], im, hdr
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
  endif

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
