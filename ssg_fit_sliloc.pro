;+
; $Id: ssg_fit_sliloc.pro,v 1.1 2003/01/14 13:49:04 jpmorgen Exp $

; ssg_fit_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_fit_slicent, indir, VERBOSE=verbose, order=order, sigma_cut=sigma_cut, $
                    width=width, noninteractive=noninteractive, write=write

;  ON_ERROR, 2
  cd, indir
  if N_elements(order) eq 0 then order=0

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind(string("dir=", indir))
  dbext, entries, "fname, nday, date, bad, m_sli_cent, e_sli_cent, sli_cent", $
         files, ndays, dates, badarray, m_sli_cents, e_sli_cents, sli_cents

  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)

  ;; Don't work directly with m_sli_cents since we write them to the
  ;; header below.  Instead work with mf (measured to fit) _sli_cents
  mf_sli_cents= m_sli_cents
  
  bad_idx = where(badarray ge 4096, count)
  if count gt 0 then begin
     overlap = where(finite(mf_sli_cents[bad_idx]), count)
     if count gt 0 then begin
        answer = 'N'
        if NOT keyword_set(noninteractive) then begin
           for ki = 0,1000 do flush_input = get_kbrd(0)
           repeat begin
              message, /CONTINUE, 'There are some bad files with measured slicer centers.  Would you like to see them again?(Y/[N])'
              answer = get_kbrd(1)
              if byte(answer) eq 10 then answer = 'N'
              answer = strupcase(answer)
           endrep until answer eq 'Y' or answer eq 'N'
           for ki = 0,1000 do flush_input = get_kbrd(0)
        endif
        if answer eq 'N' then begin
           mf_sli_cents[bad_idx[overlap]] = !values.f_nan
        endif
     endif
  endif

  coefs=jpm_polyfit(ndays-this_nday, mf_sli_cents, order, $
                    measure_error=e_sli_cents, $
                    title=string('Slicer pattern center ', indir), $
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
  sli_cents[*] = 0.
  for ci=0,order do begin
     sli_cents = sli_cents + coefs[ci]*(ndays-this_nday)^ci
  endfor
  sli_cents = float(sli_cents)
  ;; But only keep it where the error bars are negative
  meas_idx = where(e_sli_cents gt 0 and finite(mf_sli_cents), count)
  if count gt 0 then $
    sli_cents[meas_idx] = mf_sli_cents[meas_idx]

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
     dbupdate, entries, 'sli_cent', sli_cents
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
           im = readfits(files[i], hdr, silent=silent) ; Just reading fits header
           sxaddhist, string('(ssg_fit_slicent.pro) ', systime(/UTC), ' UT'), hdr
           sxaddpar, hdr, 'M_SLICEN', m_sli_cents[i], 'Measured slicer center'
           sxaddpar, hdr, 'E_SLICEN', e_sli_cents[i], 'Measured slicer center error'
           sxaddpar, hdr, 'SLI_CENT', sli_cents[i], 'Predicted slicer center'
           writefits, files[i], im, hdr
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
  endif

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
