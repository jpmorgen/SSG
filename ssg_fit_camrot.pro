;+
; $Id: ssg_fit_camrot.pro,v 1.6 2003/06/11 18:16:34 jpmorgen Exp $

; ssg_fit_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_fit_camrot, indir, VERBOSE=verbose, order=order, $
                    width=width, noninteractive=noninteractive, write=write

;  ON_ERROR, 2
  cd, indir
  if N_elements(order) eq 0 then order=0
  if NOT keyword_set(width) then width=3

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind(string("dir=", indir))
  dbext, entries, 'fname, bad, nday, date, m_cam_rot, e_cam_rot, cam_rot, no_fit', $
         files, badarray, ndays, dates, m_cam_rots, e_cam_rots, cam_rots, no_fits
  angles = m_cam_rots
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)

  ;; Don't work directly with m_cam_rot since we write them to the FITS
  ;; header below.  Instead work with mf_cam_rot (measured to fit)
  mf_cam_rots= m_cam_rots

  ;;  Check to see if user wants to redisplay frames marked as bad
  bad_idx = where(badarray ge 2048, count)
  if count gt 0 then begin
     good_idx = where(finite(mf_cam_rots[bad_idx]) eq 1, count)
     if count gt 0 then begin
        answer = 'N'
        if NOT keyword_set(noninteractive) then begin
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'There are some bad files with measured camera rotations.  Consider them for fitting? (Y/[N])'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'N'
              answer = strupcase(answer)
           endrep until answer eq 'Y' or answer eq 'N'
           for ki = 0,1000 do flush_input = get_kbrd(0)
        endif
        if answer eq 'N' then begin
           mf_cam_rots[bad_idx] = !values.f_nan
        endif
     endif
  endif

  ;;  Check to see if points were excluded before 
  bad_idx = where((no_fits AND 4) gt 0, count)
  if count gt 0 then begin
     message, /CONTINUE, 'NOTE: Previous camera rotation fit was performed by excluding some points.  In the menu, select "resurect All points" to see these points.'
  endif

  coefs=jpm_polyfit(ndays-this_nday, mf_cam_rots, order, bad_idx=bad_idx, $
                    measure_error=e_cam_rots, $
                    title=string('Camera rotation angles in ', indir), $
                    xtitle=string('UT time (Hours) ', utdate), $
                    ytitle='Angle (degrees)', $
                    xtickunits='Hours', noninteractive=noninteractive, /MJD)
                    

  ;; KEY ASSUMPTION!  I have found that for the most part, the
  ;; variation in the slicer centers (and rotations, for that matter)
  ;; are quite well determined most of the time and the only reason
  ;; for replacing them with a polynomial fit is when they are not
  ;; well determined.

  ;; Go ahead and calculate polynomial fit everywhere
  cam_rots[*] = 0.
  for ci=0,order do begin
     cam_rots = cam_rots + coefs[ci]*(ndays-this_nday)^ci
  endfor
  cam_rots=float(cam_rots)
  ;; But only keep it where the error bars are negative
  meas_idx = where(e_cam_rots gt 0 and finite(m_cam_rots) eq 1, count)
  if count gt 0 then $
    cam_rots[meas_idx] = m_cam_rots[meas_idx]
  ;; And record which values were not fit in no_fit for future
  ;; reference
  bad_idx = where(finite(mf_cam_rots) eq 0, count)
  if count gt 0 then $
    no_fits[bad_idx] = no_fits[bad_idx] OR 4


  if NOT keyword_set(noninteractive) and $
    NOT keyword_set(write) then begin
     for ki = 0,1000 do flush_input = get_kbrd(0)
     repeat begin
        message, /CONTINUE, 'Write this fit to the database and FITS headers?([Y]/N)'
        answer = get_kbrd(1)
        if byte(answer) eq 10 then answer = 'Y'
        answer = strupcase(answer)
     endrep until answer eq 'Y' or answer eq 'N'
     for ki = 0,1000 do flush_input = get_kbrd(0)
     if answer eq 'Y' then write=1
  endif

  ;; Now write the information to the FITS headers
  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'cam_rot, no_fit', cam_rots, no_fits
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
           im = ssgread(files[i], hdr, eim, ehdr)
           sxaddhist, string('(ssg_fit_camrot.pro) ', systime(/UTC), ' UT'), hdr
           sxaddhist, string('(ssg_fit_sliloc.pro) Added *CAM* keywords.  Image not modified'), hdr
           sxaddpar, hdr, 'M_CAM_ROT', m_cam_rots[i], 'Measured camera rotation (clockwise)'
           sxaddpar, hdr, 'E_CAM_ROT', e_cam_rots[i], 'Measured camera rotation error (negative = bad measurement)'
           sxaddpar, hdr, 'CAM_ROT', cam_rots[i], 'Best camera rotation'
           sxaddpar, hdr, 'NO_FIT', no_fits[i], 'Bitmap flag indicating use in polynomial fits'
;           for oi=0,order do begin
;              sxaddpar, hdr, string(format='("CAM_RP", i1)', oi), coefs[oi], 'poly. coef. in nday, origin= fix(nday,0)'
;           endfor
           ssgwrite, files[i], im, hdr, eim, ehdr
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
  endif

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
