;+
; $Id: ssg_extract.pro,v 1.4 2003/03/10 18:29:52 jpmorgen Exp $

; ssg_extract extract 1D spectra from a directory full of SSG images

;-

pro ssg_extract, indir, tv=tv, showplots=showplots, sn=sn, noninteractive=noninteractive, write=writem, winnum=winnum

;  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0
  if NOT keyword_set(sn) then sn = 10.
  if NOT keyword_set(winnum) then winnum=6

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
  rdbname = 'ssg_reduce'
  fdbname = ['oi_6300_fit', 'oi_6300_fit_ext']

  dbopen, rdbname, 0
;  entries = dbfind("typecode=[2,5]", $
  entries = dbfind("typecode=5", $
                   dbfind("bad<2047", $ ; < is really <=
                          dbfind(string("dir=", indir))))

  dbext, entries, "fname, nday, date, typecode, bad, nbad, ncr, wavelen, spectrum, spec_err, cross_disp, cross_err", files, ndays, dates, typecodes, badarray, nbads, ncrs, wavelengths, spectra, spec_errors, cross_disps, cross_errors

  dbext, entries, "med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross", med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps

  dbext, entries, "m_dispers, dispers", m_disperss, disperss

  
  files=strtrim(files)
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990,0)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(fix(ndays))     ; presumably this will throw out anything taken at an odd time

  if keyword_set(showplots) then window,winnum, $
    title='Extracted Spectrum'

  wavelengths  [*] = !values.f_nan
  spectra      [*] = !values.f_nan
  spec_errors  [*] = !values.f_nan
  cross_disps  [*] = !values.f_nan
  cross_errors [*] = !values.f_nan
  med_specs    [*] = !values.f_nan
  av_specs     [*] = !values.f_nan
  min_specs    [*] = !values.f_nan
  max_specs    [*] = !values.f_nan
  med_xdisps   [*] = !values.f_nan
  av_xdisps    [*] = !values.f_nan
  min_xdisps   [*] = !values.f_nan
  max_xdisps   [*] = !values.f_nan

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
           fname=temp[0]+'educed.fits'
        endif
        shortfile= strmid(fname, $
                          strpos(fname, '/', /REVERSE_SEARCH) + 1)

        im = ssgread(fname, hdr, eim, ehdr, /DATA, /TRIM)
        im = im + ssg_edge_mask(im, hdr)
        asize=size(im) & nx=asize[1] & ny=asize[2]
 
       if keyword_set(TV) then display, im, hdr, /reuse
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


        ;; In order to correct for flux missing from cosmic ray hits,
        ;; we need to flatten the spectrum in the cross dispersion
        ;; direction.  This means multiplying up dim pixels, so we
        ;; should get their uncertainties too.

        err2_im = eim^2

        
        ;; Now make a template to of the cross-dispersion spectrum,
        ;; save its error off for writing to the database
        ssg_spec_extract, im, hdr, junk, xdisp, /AVERAGE
        xdisp_err = sqrt(xdisp)

        ;; Normalize it and get its length so we can can correct for
        ;; missing pixles
        good_idx = where(finite(xdisp) eq 1, nxpts)
        norm_xdisp = xdisp/(total(xdisp,/NAN)/nxpts)

        template = template_create(im, norm_xdisp)

        ;; Here is the crucial code that gets all pixels to the same level:
        im = im/template
        err2_im = err2_im/template
        
        wait, 0.5
        display, im, /reuse
        wait, 0.5
        ;; Now we want to do the signal to noise calculation on the
        ;; cumulative some of the columns.  

        xaxis = fltarr(nx)
        av_spec = fltarr(nx)
        err_spec = fltarr(nx)
        ngood_pix = fltarr(nx)

        ngood_disp = 0
        last_good_disp = 0
        gap = 0
        marked_bad = 0
        for di = 0, nx-1 do begin
           column = fltarr(ny)
           e2col = fltarr(ny)
           column = im[di,*]
           e2col = err2_im[di,*]
           good_idx = where(finite(column) eq 1 and $
                            finite(e2col) eq 1, ngood_xd)
           ;; Don't even bother adding a point if there is no data for
           ;; it.  MPFITFN can handle unequally spaced X axes, but do
           ;; raise a wanting since some of my routines might not like
           ;; it
           is = 0               ; sort index and pixel counter
           if ngood_xd gt 2 then begin
              ;; Dispersion axis
              wl = 0.
              for dci = 0, order do begin
                 wl = wl + dispers[dci]*(di-nx/2.)^dci
              endfor
              ;; Optimum signal to noise calculation
              ;; But first we have to rid of NANs so that descending
              ;; sort doesn't cause problems
              temp = column[good_idx] & column = temp
              temp = e2col[good_idx]  & e2col = temp
              sort_idx = sort(e2col)
              signal = column[sort_idx[is]]
              err2 = e2col[sort_idx[is]]
              noise = column[sort_idx[is+1]]
;              while is lt ngood_xd-2 and $
;                noise gt sqrt(err2)/sn do begin
;                 is = is + 1
;                 signal = signal + noise
;                 err2 = err2 + e2col[sort_idx[is+1]]
;                 noise = column[sort_idx[is+1]]
;              endwhile
              if is eq 0 then begin
                ;; Just accept the whole column
                signal = total(column[good_idx])
                err2 = total(e2col[good_idx])
                is = ngood_xd
                ;; --> This might be wrong
                ;;last_good_disp = ngood_disp
              endif
              ;; Tuck these into the spectrum and move to the next
              ;; good element
              xaxis[ngood_disp] = wl
              av_spec[ngood_disp] = signal/is
              err_spec[ngood_disp] = sqrt(err2)/is
              last_good_disp = ngood_disp
              ngood_disp = ngood_disp + 1
         endif else begin ;; column is almost all NANS
              ;; This skips the first channels if they are bad
              if keyword_set(last_good_disp) then begin
                 ;; This skips the last channels if they are bad
                 if last_good_disp lt ngood_disp-1 then begin
                    if keyword_set(gap) then begin
                       message,'WARNING: wavelength disontinuity between channel ' + string(last_good_disp) + ' and ' +  string(last_good_disp + 1) , /CONTINUE
                       if NOT keyword_set(marked_bad) then begin
                          badarray[i] = badarray[i] OR 1024 
                          marked_bad = 1
                       endif
                       gap = 0
                    endif else begin
                       gap = 1
                    endelse
                 endif     
              endif
           endelse
           ngood_pix[di] = is
        endfor
        ngood_disp = ngood_disp - 1

        ;; Shorten arrays and put back in terms of total signal
        temp = 	xaxis	[0:ngood_disp-1]  &  xaxis = temp
        temp = 	av_spec	[0:ngood_disp-1]  &  av_spec = temp
        temp = 	err_spec[0:ngood_disp-1]  &  err_spec = temp
        spec = av_spec * nxpts
        err_spec = err_spec * nxpts

        ;; Divide by exposure time
        spec = spec/exptime
        err_spec = err_spec/exptime
        xdisp = xdisp/exptime
        xdisp_err = xdisp_err/exptime

        ;; Put these into the array that will be loaded into the database
        wavelengths	[0:ngood_disp-1, i] = xaxis
        spectra		[0:ngood_disp-1, i] = spec
        spec_errors	[0:ngood_disp-1, i] = err_spec

        ;; For now just dump the cross dispersion spectra in.
        ;; --> Eventually, I want to put them in relative to their
        ;; absolute CCD coordinates, or something like that
        ngood_xdisp = N_elements(xdisp)
        cross_disps 	[0:ngood_xdisp-1, i] = xdisp
        cross_errors	[0:ngood_xdisp-1, i] = xdisp_err

        med_specs    [i] = median(spec)
        av_specs     [i] = mean(spec, /NAN)
        min_specs    [i] = min(spec, /NAN)
        max_specs    [i] = max(spec, /NAN)
        med_xdisps   [i] = median(xdisp)
        av_xdisps    [i] = mean(xdisp, /NAN)
        min_xdisps   [i] = min(xdisp, /NAN) 
        max_xdisps   [i] = max(xdisp, /NAN) 

        if keyword_set(showplots) then begin
           !p.multi=[0,0,2]
           wset,winnum
           ;; This has to be in font !3 for the angstrom symbol to be
           ;; found
           title=string(object, ' ', shortfile, ' ', sxpar(hdr, 'DATE'))
           xtitle='Wavelength ('+string("305B)+')' ;"
           ytitle=string('Signal (', sxpar(hdr, 'BUNIT'), '/S)')
           plot, xaxis, spec, psym=dot, $
                 title=fname, xtitle=xtitle, ytitle=ytitle
           oploterr, xaxis, spec, err_spec, dot
           plot, ngood_pix, title=('Good pixel count, max = ' + string(ngood_xd)), $
                 xtitle='Column number', ytitle='number of pixels used', $
                 yrange=[0,ngood_xd], ystyle=2           
           !p.multi=0
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
           printf, lun, xaxis[id], spec[id], err_spec[id]
        endfor
        close, lun
        free_lun, lun

        ngood_files = ngood_files + 1

     endelse ;; CATCH if err
  endfor  ;; Al extractions

  CATCH, /CANCEL

  if ngood_files eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  dbclose                       ; just in case
  if NOT keyword_set(noninteractive) then begin
     dbopen, rdbname, 0
     marked_ndays = ssg_mark_bad(ndays, [[med_specs], [av_specs]], $
                                 title=string('Continuum levels and bad pixels ', indir), $
                                 xtickunits='Hours', $
                                 xtitle=string('UT time (Hours) ', utdate), $
                                 ytitle='Spectral value (electrons/s)', $
                                 window=7, legend=['Median spectral value', $
                                                   'Average spectral value'], $
                                 /MJD)
     
     dbclose
     
     bad_idx = where(finite(marked_ndays) eq 0, count)
     if count gt 0 then badarray[bad_idx] = badarray[bad_idx] OR 512
     
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
     dbopen, rdbname, 1
     dbupdate, entries, 'bad, wavelen, spectrum, spec_err, cross_disp, cross_err, med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross', badarray, wavelengths, spectra, spec_errors, cross_disps, cross_errors, med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps
     dbclose

     ;; Now let's initialize the fitting database
     oldpriv=!priv
     !priv = 2
     dbopen, fdbname[0], 0

     ;; Trying to use fancy list-building code
     for i=0, ngood_files-1 do begin
        reinit = where_nday_eq(ndays[i], COUNT=count,SILENT=silent)
        if count eq 1 then begin
           ;; record a list of entries to reinitialize
           if N_elements(reinit_list) eq 0 then begin
              reinit_list = reinit
              new_spec_val = 1
           endif else begin
              reinit_list = [reinit_list, reinit]
              new_spec_val = [new_spec_val, 1]
           endelse
        endif else begin
           ;; record a list of new ndays to add
           if N_elements(build_list) eq 0 then begin
              build_list = ndays[i]
              blnew_spec_val = 1
           endif else begin
              build_list = [build_list, ndays[i]]
              blnew_spec_val = [blnew_spec_val, 1]
           endelse
        endelse
        if N_elements(good_ndays) eq 0 then $
          good_ndays = ndays[i] $
        else $
          good_ndays = [good_ndays, ndays[i]]
     endfor
     dbclose
 
     ;; Build or update the fitting database with just a couple of
     ;; columns at first and prepare to pull the rest of the stuff
     ;; over from reduction database
     dbopen, fdbname[0], 1
     if N_elements(reinit_list) gt 0 then begin
         dbupdate, reinit_list, 'new_spec', new_spec_val
     endif
     if N_elements(build_list) gt 0 then begin
         dbbuild, build_list, blnew_spec_val
     endif
     dbclose
     dbopen, fdbname[1], 1
     if N_elements(build_list) gt 0 then begin
         dbbuild, build_list
     endif
     dbclose

     dbopen, rdbname, 0
     rentries = where_nday_eq(good_ndays, COUNT=count,SILENT=silent)
;     dbext, rentries, "bad, wavelen, spectrum, spec_err, cross_disp, cross_err", badarray, wavelengths, spectra, spec_errors, cross_disps, cross_errors
;     dbext, rentries, "med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross", med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps
     dbext, rentries, "bad, m_dispers, dispers", badarray, m_disperss, disperss
     dbclose

     dbopen, fdbname[0], 1
     fentries = where_nday_eq(good_ndays, SILENT=silent)
;     dbupdate, fentries, "bad, wavelen, spectrum, spec_err, cross_disp, cross_err", badarray, wavelengths, spectra, spec_errors, cross_disps, cross_errors
;     dbupdate, fentries, "med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross", med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps

     ;; (re)initialize fit_vers
     fit_vers=bytarr(N_elements(fentries))
     dbupdate, fentries, "bad, fit_vers, m_dispers, dispers", badarray, fit_vers, m_disperss, disperss

     dbclose

     !priv=oldpriv
     message, /INFORMATIONAL, 'Wrote spectra to and associated info to ' + rdbname + ' and initialized ' + fdbname[0] + ' and ' + fdbname[1] + '.  Run ssg_fit next'
  endif

     ;; For convenience 
     message, /INFORMATIONAL, 'Directory is set to ' + indir

end

