;+
; $Id: ssg_extract.pro,v 1.2 2002/11/28 17:48:21 jpmorgen Exp $

; ssg_extract extract 1D spectra from a directory full of SSG images

;-

pro ssg_extract, indir, tv=tv, showplots=showplots, sn=sn

;  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0
  if NOT keyword_set(sn) then sn = 10.

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
  entries = dbfind("typecode=[2,5]", $
                   dbfind("bad<2047", $ ; < is really <=
                          dbfind(string("dir=", indir))))

  dbext, entries, "fname, nday, date, typecode, bad, nbad, wavelen, spectrum, spec_err, cross_disp, cross_err", files, ndays, dates, typecodes, badarray, nbads, wavelengths, spectra, spec_errors, cross_disps, cross_errors
  
  files=strtrim(files)
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(ndays)     ; presumably this will throw out anything taken at an odd time

  if keyword_set(showplots) then window,7

  ngood_files = 0
  err = 0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; Eventually I'd like to process everything in place, but
        ;; since taking out the cosmic rays is time consuming, I have
        ;; done it this way.
        fname = files[i]
        if typecodes[i] eq 5 then begin
           temp=strsplit(files[i], '.fits', /extract)
           fname=temp[0]+'.red.fits'
        endif
        shortfile= strmid(fname, $
                          strpos(fname, '/', /REVERSE_SEARCH) + 1)

        im = ssgread(fname, hdr)
        asize=size(im) & nx=asize[1] & ny=asize[2]

        ;; Get exposure time
        exptime = sxpar(hdr, 'EXPTIME', count=count)
        if count eq 0 then $
          message, 'ERROR: no exposure time found in FITS header'

        object = sxpar(hdr, 'OBJECT', count=count)
        if count eq 0 then $
          message, 'WARNING: no OBJECT keyword found in FITS header', /CONTINUE

        ;; Read in dispersion coefficients
        order = 0
        dispers = sxpar(hdr, $
                        string(format='("DISPERS", i1)', order), count=count)
        while count ne 0 do begin
           order = order + 1
           temp = sxpar(hdr, $
                        string(format='("DISPERS", i1)', order), count=count)
           dispers = [dispers, temp]
        endwhile
        if order eq 0 then $
          message, 'ERROR: no DISPERS keyword found in header of ' + fname


        xaxis = fltarr(nx)
        spec = fltarr(nx)
        err_axis = fltarr(nx)

        ngood_disp = 0
        last_good_disp = 0
        gap = 0
        marked_bad = 0
        for di = 0, nx-1 do begin
           column = fltarr(ny)
           column = im[di,*]
           good_idx = where(finite(column) eq 1, ngood_xd)
           ;; Don't even bother adding a point if there is no data for
           ;; it.  MPFITFN can handle unequally spaced X axes, but do
           ;; raise a wanting since some of my routines might not like
           ;; it
           if ngood_xd gt 2 then begin
              ;; Dispersion axis
              wl = 0.
              for dci = 0, order do begin
                 wl = wl + dispers[dci]*(di-nx/2.)^dci
              endfor
              ;; Optimum signal to noise calculation
              ;; But first we have to rid of NANs so that descending
              ;; sort doesn't cause problems
              temp = column[good_idx]
              column = temp
              sort_idx = sort(column)
              sort_idx = reverse(sort_idx)
              is = 0            ; sort index
              signal = column[sort_idx[is]]
              error = sqrt(signal)
              noise = column[sort_idx[is+1]]
              while is lt ngood_xd-2 and $
                noise gt error/sn do begin
                 is = is + 1
                 signal = signal + noise
                 error = sqrt(signal)
                 noise = column[sort_idx[is+1]]
              endwhile
              ;; Tuck these into the spectrum and move to the next
              ;; good element
              xaxis[ngood_disp] = wl
              spec[ngood_disp] = signal
              err_axis[ngood_disp] = error
              last_good_disp = ngood_disp
              ngood_disp = ngood_disp + 1
           endif else begin
              ;; This skips the first channels if they are bad
              if keyword_set(last_good_disp) then begin
                 ;; This skips the last channels if they are bad
                 if last_good_disp lt ngood_disp-1 then begin
                    if keyword_set(gap) then begin
                       message,'WARNING: wavelength disontinuity between channel ' + string(last_good_disp) + ' and ' +  string(last_good_disp + 1) , /CONTINUE
                       if NOT keyword_set(marked_bad) then begin
                          badarray[i] = badarray[i] + 1024 
                          marked_bad = 1
                       endif
                       gap = 0
                    endif else begin
                       gap = 1
                    endelse
                 endif     
              endif
           endelse
        endfor
        ngood_disp = ngood_disp - 1

        temp = 	xaxis	[0:ngood_disp-1]
        xaxis = temp
        temp = 	spec	[0:ngood_disp-1]
        spec = temp
        temp = 	err_axis[0:ngood_disp-1]
        err_axis = temp

        ;; We don't need to be so careful about the cross-dispersion
        ;; spectra, since the signal on them is huge
        ;; --> I could take the code above + make a funciton out of it
        ;; + pass it the rotated array to get the corss dispersion
        ;; spectrum.  But for now this will do.
        ssg_spec_extract, im, hdr, junk, xdisp, /AVERAGE
        xdisp_err = sqrt(xdisp)

        spec = spec/exptime
        err_axis = err_axis/exptime
        xdisp = xdisp/exptime
        xdisp_err = xdisp_err/exptime

        wavelengths	[0:ngood_disp-1, i] = xaxis
        spectra		[0:ngood_disp-1, i] = spec
        spec_errors	[0:ngood_disp-1, i] = err_axis

        ;; For now just dump the cross dispersion spectra in.
        ;; --> Eventually, I want to put them in relative to their
        ;; absolute CCD coordinates, or something like that
        ngood_xdisp = N_elements(xdisp)
        cross_disps 	[0:ngood_xdisp-1, i] = xdisp
        cross_errors	[0:ngood_xdisp-1, i] = xdisp_err

        if keyword_set(showplots) then begin
           wset,7
           ;; This has to be in font !3 for the angstrom symbol to be
           ;; found
           title=string(object, ' ', shortfile, ' ', sxpar(hdr, 'DATE'))
           xtitle='Wavelength ('+string("305B)+')' ;"
           ytitle=string('Signal (', sxpar(hdr, 'BUNIT'), '/S)')
           plot, xaxis, spec, psym=dot, $
                 title=fname, xtitle=xtitle, ytitle=ytitle
           oploterr, xaxis, spec, err_axis, dot
        endif

        if keyword_set(TV) then $
          display, im, hdr, /reuse

        ;; For Voigt fit
        xaxis = xaxis - 6300
        
        temp=strsplit(files[i], '.fits', /extract)
        fname=temp[0]+'.spe'
        message, /INFORMATIONAL, 'Writing ' + fname
        
        openw, lun, fname, /get_lun
        for id = 0,ngood_disp-1 do begin
           printf, lun, xaxis[id], spec[id], err_axis[id]
        endfor
        close, lun, /ALL

        ngood_files = ngood_files + 1

     endelse ;; CATCH if err
  endfor  ;; Al extractions

  CATCH, /CANCEL

  if ngood_files eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  med_specs = fltarr(nf)
  for i = 0,nf-1 do begin
     med_specs[i] = median(spectra[*,i])
  endfor

  dbclose ; just in case
  dbopen, dbname, 0
  marked_ndays = ssg_mark_bad(ndays, [[med_specs], [nbads/100.]], $
                              title=string('Continuum levels and bad pixels ', indir), $
                              xtickunits='Hours', $
                              xtitle=string('UT time (Hours) ', utdate), $
                              ytitle='Median spectrum value (electrons/s), number of bad pixels/100', $
                              window=7, legend=['Median spectral value', 'NUmber of bad pixels'])

  dbclose
  
  bad_idx = where(finite(marked_ndays) eq 0, count)
  if count gt 0 then badarray[bad_idx] = badarray[bad_idx] + 512

  write=0
  if NOT keyword_set(noninteractive) then begin
     if NOT keyword_set(write) then begin
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'Write these spectra to the database, erasing previous versions?(Y/[N])'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'N'
           answer = strupcase(answer)
        endrep until answer eq 'Y' or answer eq 'N'
        for ki = 0,1000 do flush_input = get_kbrd(0)
     endif
     if answer eq 'Y' then write=1
  endif

  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'bad, wavelen, spectrum, spec_err, cross_disp, cross_err', badarray, wavelengths, spectra, spec_errors, cross_disps, cross_errors
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Wrote spectra to ' + dbname + '.  Run ssg_fit_spec next'
  endif

     ;; For convenience 
     message, /INFORMATIONAL, 'Directory is set to ' + indir

end

