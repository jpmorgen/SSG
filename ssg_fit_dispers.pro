;+
; $Id: ssg_fit_dispers.pro,v 1.1 2002/12/16 13:38:12 jpmorgen Exp $

; ssg_fit_dispers.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_fit_dispers, indir, VERBOSE=verbose, order=order, sigma_cut=sigma_cut, $
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
  dbext, entries, "fname, nday, date, m_dispers, wavelen", files, ndays, dates, m_dispers, dispers
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(ndays)     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)

  refit=1
  if keyword_set(noninteractive) then refit = 0
  window,7

  asize = size(m_dispers)
  if asize[0] lt 2 then message, 'ERROR: improperly formatted database--make this code and ssg_db_create consistent'

  max_order=0
  for i=0,nf-1 do begin
     junk = where(finite(m_dispers[*,i]) and m_dispers[*,i] ne 0, count)
     if count gt max_order then max_order = count
  endfor
  if max_order eq 0 then message, 'ERROR: No dispersion coefficients found.  Did you run ssg_get_dispers?  Did you fix up the database creation code?'
  m_dispers(where(m_dispers eq 0)) = !values.f_nan

  ;; Fit everything and put the answer into dispers
  for ido=0, max_order-1 do begin
     coefs=jpm_polyfit(ndays-this_nday, $
                       m_dispers[ido,*], order, $
                       title=string('Dispersion coefficient ', ido), $
                       xtitle=string('UT time (Hours) ', utdate), $
                       ytitle='Coefficient value', $
                       xtickunits='Hours', noninteractive=noninteractive)
     
     dispers[ido,*] = 0
     for ci=0,order do begin    ; That is order of fit vs time, not dispersion order
        dispers[ido,*] = dispers[ido,*] $
          + coefs[ci]*(ndays-this_nday)^ci
     endfor
  endfor


  if NOT keyword_set(noninteractive) then begin
     if NOT keyword_set(write) then begin
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'Write this fit to the database and FITS headers?([Y]/N)'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'Y'
           answer = strupcase(answer)
        endrep until answer eq 'Y' or answer eq 'N'
        for ki = 0,1000 do flush_input = get_kbrd(0)
     endif
     if answer eq 'Y' then write=1
  endif

  ;; Now write the information to the FITS headers
  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'wavelen', dispers
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated dispersion coefs in ' + dbname

     err=0
     message, /INFORMATIONAL, 'Writing values in FITS headers'
     for i=0,nf-1 do begin
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           im = readfits(files[i], hdr, silent=silent) ; Just reading fits header
           sxaddhist, string('(ssg_fit_dispers.pro) ', systime(/UTC), ' UT'), hdr
           for ido = 0, max_order - 1 do begin
                 sxaddpar, hdr, $
                           string(format='("DISPERS", i1)', ido), $
                           dispers[ido,i], $
                           'Dispersion coef (ref to image center)'
           endfor
           writefits, files[i], im, hdr
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
  endif

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
