;+
; $Id: ssg_fit_sliloc.pro,v 1.3 2003/06/11 18:09:17 jpmorgen Exp $

; ssg_fit_sliloc.  Fit the slicer location parameters to find good centers

;-

pro ssg_fit_sliloc, indir, VERBOSE=verbose, flat_only=flat_only, order=order, $
                    noninteractive=noninteractive, write=write

;  ON_ERROR, 2
  cd, indir
  if N_elements(order) eq 0 then order=0

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind(string("dir=", indir))
  dbext, entries, 'fname, nday, date, bad, typecode', $
         files, ndays, dates, badarray, typecodes
  dbext, entries, 'm_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, no_fit', $
         m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, no_fits

  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)

  ;; Don't work directly with m_sli_bots and tops since we write them to the
  ;; header below.  Instead work with mf (measured to fit) _sli_*
  mf_sli_bots= m_sli_bots
  mf_sli_tops= m_sli_tops
  
  ;;  Check to see if user wants to redisplay frames marked as bad
  bad_idx = where(badarray ge 4096, count)
  if count gt 0 then begin
     overlap = where(finite(mf_sli_bots[bad_idx]) eq 1 or $
                     finite(mf_sli_tops[bad_idx]) eq 1, count)
     if count gt 0 then begin
        answer = 'N'
        if NOT keyword_set(noninteractive) then begin
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'There are some bad files with measured slicer positions.  Consider them for fitting? (Y/[N])'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'N'
              answer = strupcase(answer)
           endrep until answer eq 'Y' or answer eq 'N'
           for ki = 0,1000 do flush_input = get_kbrd(0)
        endif
        if answer eq 'N' then begin
           mf_sli_bots[bad_idx[overlap]] = !values.f_nan
           mf_sli_tops[bad_idx[overlap]] = !values.f_nan
        endif
     endif
  endif

  ;; Handle the flat_only case
  if keyword_set(flat_only) then begin
     non_flat_idx = where(typecodes lt 3 or typecodes gt 4, count)
     if count eq 0 then begin
        message, 'WARNING: nothing but flats in ' + string(indir)
     endif else begin
        e_sli_bots[non_flat_idx] = -abs(e_sli_bots[non_flat_idx])
        e_sli_tops[non_flat_idx] = -abs(e_sli_tops[non_flat_idx])
        no_fits[non_flat_idx] = no_fits[non_flat_idx] OR 3
     endelse
  endif


  ;; BOTTOM
  ;;  Check to see if points were excluded before 
  bad_idx = where((no_fits AND 1) gt 0, count)
  if count gt 0 then begin
     message, /CONTINUE, 'NOTE: Previous slicer bottom position fit was performed by excluding some points.  In the menu, select "resurect All points" to see these points.'
  endif

  coefs=jpm_polyfit(ndays-this_nday, mf_sli_bots, order, $
                    measure_error=e_sli_bots, bad_idx=bad_idx, $
                    title=string('Bottom of Slicer Pattern ', indir), $
                    xtickunits='Hours', $
                    xtitle=string('UT time (Hours) ', utdate), $
                    ytitle='Pixels from bottom of image', $
                    noninteractive=noninteractive, /MJD)
                   
  ;; KEY ASSUMPTION!  I have found that for the most part, the
  ;; variation in the slicer centers (and rotations, for that matter)
  ;; are quite well determined most of the time and the only reason
  ;; for replacing them with a polynomial fit is when they are not
  ;; well determined.

  ;; Go ahead and calculate polynomial fit everywhere
  sli_bots[*] = 0.
  for ci=0,order do begin
     sli_bots = sli_bots + coefs[ci]*(ndays-this_nday)^ci
  endfor
  sli_bots = float(sli_bots)
  ;; Put back the good measurements
  meas_idx = where(e_sli_bots gt 0 and finite(m_sli_bots) eq 1, count)
  if count gt 0 then $
    sli_bots[meas_idx] = m_sli_bots[meas_idx]
  ;; Reset this bit on no_fits and record which values were not fit in
  ;; no_fit for future reference
  no_fits = no_fits and NOT 1
  bad_idx = where(finite(mf_sli_bots) eq 0, count)
  if count gt 0 then $
    no_fits[bad_idx] = no_fits[bad_idx] OR 1

  ;; TOP
  ;;  Check to see if points were excluded before 
  bad_idx = where((no_fits AND 2) gt 0, count)
  if count gt 0 then begin
     message, /CONTINUE, 'NOTE: Previous slicer top position fit was performed by excluding some points.  In the menu, select "resurect All points" to see these points.'
  endif

  coefs=jpm_polyfit(ndays-this_nday, mf_sli_tops, order, $
                    measure_error=e_sli_tops, bad_idx=bad_idx, $
                    title=string('Top of Slicer Pattern ', indir), $
                    xtickunits='Hours', $
                    xtitle=string('UT time (Hours) ', utdate), $
                    ytitle='Pixels from bottom of image', $
                    noninteractive=noninteractive, /MJD)
                   
  ;; Go ahead and calculate polynomial fit everywhere
  sli_tops[*] = 0.
  for ci=0,order do begin
     sli_tops = sli_tops + coefs[ci]*(ndays-this_nday)^ci
  endfor
  sli_tops = float(sli_tops)
  ;; But only keep it where the error bars are negative
  meas_idx = where(e_sli_tops gt 0 and finite(m_sli_tops) eq 1, count)
  if count gt 0 then $
    sli_tops[meas_idx] = m_sli_tops[meas_idx]
  ;; And record which values were not fit in no_fit for future
  ;; reference
  no_fits = no_fits and NOT 2
  bad_idx = where(finite(mf_sli_tops) eq 0, count)
  if count gt 0 then $
    no_fits[bad_idx] = no_fits[bad_idx] OR 2

  ;; CENTER, which is what we really want
  sli_cents = (sli_bots + sli_tops) / 2.
  e_sli_cents = sqrt(e_sli_bots^2 + e_sli_tops^2)
  ;; Propagate negative error bars to indicate we had a dubious
  ;; measurement
  bad_idx = where(e_sli_bots le 0 or e_sli_tops le 0, count)
  if count gt 0 then begin
     e_sli_cents[bad_idx] = -e_sli_cents[bad_idx]
  endif

  if NOT (keyword_set(noninteractive) and $
          keyword_set(write)) then begin
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
     dbupdate, entries, 'm_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, no_fit', $
         m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, no_fits
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated slicer center stuff in ' + dbname

     err=0
     message, /INFORMATIONAL, 'Writing values in FITS headers'
     for i=0,nf-1 do begin
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           im = ssgread(files[i], hdr, eim, ehdr)
           sxaddhist, string('(ssg_fit_sliloc.pro) ', systime(/UTC), ' UT'), hdr
           sxaddhist, string('(ssg_fit_sliloc.pro) Added *SLI* keywords.  Image not modified'), hdr
           sxaddpar, hdr, 'SLI_BOT', sli_bots[i], 'Slicer bottom edge, pix from bot of image'
           sxaddpar, hdr, 'M_SLIBOT', m_sli_bots[i], 'Measured slicer bottom'
           sxaddpar, hdr, 'E_SLIBOT', e_sli_bots[i], 'Measured slicer bottom error'
           sxaddpar, hdr, 'SLI_TOP', sli_tops[i], 'Slicer top edge'
           sxaddpar, hdr, 'M_SLITOP', m_sli_tops[i], 'Measured slicer top'
           sxaddpar, hdr, 'E_SLITOP', e_sli_tops[i], 'Measured slicer top error'
           sxaddpar, hdr, 'NO_FIT', no_fits[i], 'Bitmap flag indicating use in polynomial fits'
           sxaddpar, hdr, 'SLI_CENT', sli_cents[i], 'Best slicer center'
           sxaddpar, hdr, 'E_SLICEN', e_sli_cents[i], 'Slicer center error'
           ssgwrite, files[i], im, hdr, eim, ehdr
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
  endif

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
