;+
; $Id: ssg_extract.pro,v 1.1 2002/11/28 15:56:04 jpmorgen Exp $

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

  dbext, entries, "fname, typecode, spectrum, spec_err", files, typecodes, spectra, spec_errors
  dbclose
  
  files=strtrim(files)
  nf = N_elements(files)

  if keyword_set(showplots) then window,7

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
        for di = 0, nx-1 do begin
           column = fltarr(ny)
           column = im[di,*]
           good_idx = where(finite(column) eq 1, ngood_xd)
           ;; Don't even bother adding a point if there is no data for
           ;; it.  MPFITFN can handle unequally spaced X axes
           if ngood_xd gt 2 then begin
              ;; Dispersion axis
              wl = 0.
              for dci = 0, order do begin
                 wl = wl + dispers[dci]*(di-nx/2.)^dci
              endfor
              ;; Optimum signal to noise calculation
              ;; First get rid of NANs
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

              ngood_disp = ngood_disp + 1
           endif
        endfor
        ngood_disp = ngood_disp - 1

        temp = xaxis	[0:ngood_disp-1]
        xaxis = temp
        temp = spec	[0:ngood_disp-1]
        spec = temp
        temp = err_axis	[0:ngood_disp-1]
        err_axis = temp

        spec = spec/exptime
        err_axis = err_axis/exptime

        if keyword_set(showplots) then begin
           wset,7
           ;; This has to be in font !3 for the angstrom symbol to be
           ;; found
           title=string(object, ' ', shortfile, ' ', sxpar(hdr, 'DATE'))
           xtitle='Wavelength ('+string("305B)+')' ;"
           ytitle=string('Signal (', sxpar(hdr, 'BUNIT'), '/S)')
           plot, xaxis, spec, psym=dot, $
                 title=fname, xtitle=xtitle, ytitle=ytitle
           ploterr, xaxis, spec, err_axis, psym=dot
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
     endelse


  endfor

  CATCH, /CANCEL

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end


