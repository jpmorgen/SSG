;+
; $Id: ssg_fit_slicer.pro,v 1.1 2002/11/20 16:20:23 jpmorgen Exp $

; ssg_fit_slicer.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_fit_slicer, indir, VERBOSE=verbose, order=order, sigma_cut=sigma_cut, $
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
  dbext, entries, "fname, nday, date, m_slice, slice", files, ndays, dates, m_slicers, slicers
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(ndays)     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)

  refit=1
  if keyword_set(noninteractive) then refit = 0
  window,7

  asize = size(m_slicers)
  if asize[0] lt 2 then message, 'ERROR: improperly formatted database--make this code and ssg_db_create consistent'
  square_test = sqrt(asize[1])
  if square_test ne fix(square_test) then message, 'ERROR: I can only deal with a square array of coefficients'

  npxd = 0
  npd = 0

  ;; Get the maximum number of cross-dispersion coefficients used
  repeat begin
     good_idx = where(finite(m_slicers[npxd,*]), count)
     npxd = npxd + 1
  endrep until count eq 0
  npxd =  npxd - 1
  ;; Get the maximum number of dispersion direction coefficients used
  repeat begin
     good_idx = where(finite(m_slicers $
                             [npd*square_test:(npd+1)*square_test-1,*]), count)
     npd = npd + 1
  endrep until count eq 0
  npd = npd - 1

  if (npxd eq square_test and npd eq square_test) $
    or (npxd eq 0 and npd eq 0) then $
    message, 'ERROR: you must first run ssg_get_slicer and let it finish fitting at least one spectrum'

  ;; Fit everything and put the answer into slicer
  for ipxd=0, npxd-1 do begin
     for ipd=0, npd-1 do begin
        index = ipxd + square_test*ipd
        coefs=jpm_polyfit(ndays-this_nday, $
                          m_slicers[index,*], order, $
                          title=string('Slicer coefficient ', ipxd, ipd), $
                          xtitle=string('UT time (Hours) ', utdate), $
                          ytitle='Coefficient value', $
                          xtickunits='Hours')
        
        slicers[index,*] = 0
        for ci=0,order do begin
           index = ipxd + square_test*ipd
           slicers[index,*] = slicers[index,*] $
             + coefs[ci]*(ndays-this_nday)^ci
        endfor
     endfor
  endfor


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
     dbupdate, entries, 'slice', slicers
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated slicer coefs in ' + dbname

     err=0
     message, /INFORMATIONAL, 'Writing values in FITS headers'
     for i=0,nf-1 do begin
        CATCH, err
        if err ne 0 then begin
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + files[i], /CONTINUE
        endif else begin
           im = readfits(files[i], hdr, silent=silent) ; Just reading fits header
           sxaddhist, string('(ssg_fit_slicer.pro) ', systime(/UTC), ' UT'), hdr
           sxaddhist, string('(ssg_fit_slicer.pro) sigma_cut= ', sigma_cut), hdr
           for ipxd = 0, npxd - 1 do begin
              for ipd = 0,npd-1 do begin
                 index = ipxd + square_test*ipd
                 sxaddpar, hdr, $
                           string(format='("SLICER", i1, i1)', ipd, ipxd), $
                           slicers[index,i], $
                           'Slicer shape coefficient (see ssg_slicer.pro)'
              endfor
           endfor
           writefits, files[i], im, hdr
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
  endif

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
