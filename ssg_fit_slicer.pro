;+
; $Id: ssg_fit_slicer.pro,v 1.6 2009/07/01 22:00:34 jpmorgen Exp $

; ssg_fit_slicer.  Fit J-shaped or tilted slices

;-

pro ssg_fit_slicer, indir, VERBOSE=verbose, order=order, $
                    noninteractive=noninteractive, write=write, $
                    noshape=noshape

;  ON_ERROR, 2
  cd, indir
  if N_elements(order) eq 0 then order=0

  silent = 1
  if keyword_set(verbose) then silent = 0


  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind(string("dir=", indir))
  dbext, entries, "fname, nday, date, m_slice, slice", files, ndays, dates, m_slicers, slicers
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)

  refit=1
  if keyword_set(noninteractive) then $
    refit = 0

  asize = size(m_slicers)
  if asize[0] lt 2 then message, 'ERROR: improperly formatted database--make this code and ssg_db_create consistent'
  max_npxd = asize[1]
  ;; --> Square test does not give me what I think it does.  Array is
  ;; 100 elements long from the database, so it is always 10.
  ;; --> fix this by adding npxd,npd to database or by some other means.
  square_test = sqrt(max_npxd)
  if square_test ne fix(square_test) then message, 'ERROR: I can only deal with a square array of coefficients'

  npxd = 0
  npd = 0

  ;; Get the maximum number of cross-dispersion coefficients used
  repeat begin
     good_idx = where(finite(m_slicers[npxd,*]) eq 1, count)
     npxd = npxd + 1
  endrep until count eq 0 or npxd eq max_npxd
  if npxd eq max_npxd then $
    message, 'ERROR: first run ssg_get_slicer ' + indir
  npxd =  npxd - 1
  ;; Get the maximum number of dispersion direction coefficients used
  repeat begin
     good_idx = where(finite(m_slicers $
                             [npd*square_test:(npd+1)*square_test-1,*]) eq 1, $
                      count)
     npd = npd + 1
  endrep until count eq 0
  npd = npd - 1


  if keyword_set(noshape) then begin
     npxd = 1
     npd = 1
     slicers[*] = 0
  endif
  if (npxd eq square_test and npd eq square_test) $
    or (npxd eq 0 and npd eq 0) then $
     message, 'ERROR: specify /NOSHAPE or run ssg_get_slicer and let it finish fitting at least one spectrum.'

  ;; Fit everything and put the answer into slicer
  for ipxd=0, npxd-1 do begin
     for ipd=0, npd-1 do begin
        index = ipxd + square_test*ipd
        coefs=jpm_polyfit(ndays-this_nday, $
                          m_slicers[index,*], order, $
                          title=string('Slicer coefficient ', ipxd, ipd), $
                          xtitle=string('UT time (Hours) ', utdate), $
                          ytitle='Coefficient value', $
                          xtickunits='Hours', $
                          noninteractive=noninteractive, /MJD)
        
        slicers[index,*] = 0
        for ci=0,order do begin
           index = ipxd + square_test*ipd
           slicers[index,*] = slicers[index,*] $
             + coefs[ci]*(ndays-this_nday)^ci
        endfor
     endfor
  endfor


  if NOT keyword_set(noninteractive) then begin
     if NOT keyword_set(write) then begin
        repeat begin
           message, /CONTINUE, 'Write this fit to the database and FITS headers?([Y]/N)'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'Y'
           answer = strupcase(answer)
        endrep until answer eq 'Y' or answer eq 'N'
     if answer eq 'Y' then write=1
     endif
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
           im = ssgread(files[i], hdr, eim, ehdr)
           sxaddhist, string('(ssg_fit_slicer.pro) ', systime(/UTC), ' UT'), hdr
           sxaddhist, string('(ssg_fit_slicer.pro) Added SLICER* keywords.  Image not modified'), hdr
           for ipxd = 0, npxd - 1 do begin
              for ipd = 0,npd-1 do begin
                 index = ipxd + square_test*ipd
                 sxaddpar, hdr, $
                           string(format='("SLICER", i1, i1)', ipd, ipxd), $
                           slicers[index,i], $
                           'Slicer shape coefficient (see ssg_slicer.pro)'
              endfor
           endfor
           ssgwrite, files[i], im, hdr, eim, ehdr
        endelse ;; CATCH if err
     endfor ;; all files in directory
     CATCH, /CANCEL
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
