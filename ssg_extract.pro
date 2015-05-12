;+
; $Id: ssg_extract.pro,v 1.8 2015/03/04 15:51:46 jpmorgen Exp $

; ssg_extract extract 1D spectra from a directory full of SSG images.
; For removing cosmic ray hits, assumes that the cross-dispersion
; spectrum is uniform as a function of dispersion direction.  In other
; words, that there is no sptial dependence to the spectral lines.  In
; other words, this does not work so well for Jupiter spectra unless
; they are cosmic ray free.

;-

pro ssg_extract, indir, tv=tv, showplots=showplots, sn_imp=sn_imp, min_frac=min_frac, noninteractive=noninteractive, write=writem, winnum=winnum, FTOL=ftol

;  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0
  ;; sn_imp is the signal-to-noise improvement threshold level.  The
  ;; total cross-dispersion spectrum of each image is sorted by
  ;; signal/noise (here noise=error bar).  The total signal and noise
  ;; are added up + the worst signal/noise values are subtracted to
  ;; see if that that would improve the total S/N by sn_imp.
  if NOT keyword_set(sn_imp) then sn_imp = 1.01
  ;; column must have at least this fraction of non NAN pixels to be counted
  if NOT keyword_set(min_frac) then min_frac = 0.5
  if NOT keyword_set(winnum) then winnum=2
;  if NOT keyword_set(ftol) then ftol=1E-4
  if NOT keyword_set(ftol) then ftol=1E-6
  if keyword_set(writem) then write=writem


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
                          dbfind(string("dir=", indir))), count=count)
  if count eq 0 then begin
     message, /INFORMATIONAL, 'NOTE: no object spectra recorded on ' + indir + '.  Returning without doing anything'
     dbclose
     return
  endif


  dbext, entries, "fname, object, nday, date, typecode, obj_code, bad", files, objects, ndays, dates, typecodes, obj_codes, badarray

  dbext, entries, "nbad, ncr, wavelen, spectrum, spec_err, cross_disp, cross_err", nbads, ncrs, wavelengths, spectra, spec_errors, cross_disps, cross_errors

  dbext, entries, "med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross", med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps

  dbext, entries, "disp_pix, xdisp_pix, m_dispers, dispers, n_disp, n_xdisp, cross_dcog, sli_cent", $
         disp_pix, xdisp_pix, m_disperss, disperss, n_disp, n_xdisp, cross_dcogs, sli_cents

  
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

  disp_pix      [*] = !values.f_nan
  xdisp_pix     [*] = !values.f_nan
  wavelengths   [*] = !values.f_nan
  spectra       [*] = !values.f_nan
  spec_errors   [*] = !values.f_nan
  cross_disps   [*] = !values.f_nan
  cross_errors  [*] = !values.f_nan
  med_specs     [*] = !values.f_nan
  av_specs      [*] = !values.f_nan
  min_specs     [*] = !values.f_nan
  max_specs     [*] = !values.f_nan
  med_xdisps    [*] = !values.f_nan
  av_xdisps     [*] = !values.f_nan
  min_xdisps    [*] = !values.f_nan
  max_xdisps    [*] = !values.f_nan
  n_disp      	[*] = !values.f_nan
  n_xdisp	[*] = !values.f_nan
  cross_dcogs	[*] = !values.f_nan

  ngood_files = 0
  err = 0
  for i=0,nf-1 do begin
     ;CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; Eventually I'd like to process everything in place, but
        ;; since taking out the cosmic rays is time consuming, I have
        ;; done it this way.
        fname = files[i]
        temp=strmid(files[i], 0, strpos(files[i], 'r.fits'))
        fname=temp+'clean.fits'
        shortfile= strmid(fname, $
                          strpos(fname, '/', /REVERSE_SEARCH) + 1)

        im = ssgread(fname, hdr, eim, ehdr, /DATA, /TRIM)
        ;; Since I will be working a lot with quadradure sums...
        err_im2 = eim^2
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

        
        ;; I have tried a couple of different ways to correct for flux
        ;; missing from cosmic ray hits.  I have arrived at this one
        ;; as the best method that preserves the signal-to-noise of
        ;; the spectrum in the dispersion direction.  If you have good
        ;; spectra, in which the median and the mean are similar, it
        ;; doesn't matter much.  But doing it this way makes a big
        ;; difference for the nominal data.  The idea here is to make
        ;; a cross-dispersion spectrum that is fit to each column.
        ;; Missing pixels are replaced with the fit value and an error
        ;; estimate.  Things can then be added up between sli_bot and
        ;; sli_top for the final spectrum. [This seems to be left-over
        ;; documentation for what became ssg_cr_replace.  Below is
        ;; calculating center of gravity of slicer pattern and
        ;; choosing the set of rows with the best S/N]

        ssg_spec_extract, im, hdr, rough_spec, xdisp, /AVERAGE
        ssg_spec_extract, err_im2, hdr, rough_err2, xdisp_err2, /AVERAGE

        ;; Get the good portion of the cross-dispersion spectrum and
        ;; normalize it.  
        full_xdisp_good_idx = where(finite(xdisp) eq 1 $
                                    and finite(xdisp_err2) eq 1 , nxpts, $
                                    complement=bad_idx, ncomplement=nbad)
        ;; Note that the pixel in norm_xdisp are relative to
        ;; full_xdisp_good_idx
        norm_xdisp = normalize(xdisp[full_xdisp_good_idx], factor=factor)
        norm_xdisp_err2 = xdisp_err2[full_xdisp_good_idx]*factor^2

        ;; Calculate the center of gravity of the good_xdisp and
        ;; subtract it from the slicer center to get a figure of merit
        ;; on how centered the image of Io is.
        cross_dcogs[i] = total(xdisp[full_xdisp_good_idx]*full_xdisp_good_idx, $
                               /NAN)/total(xdisp[full_xdisp_good_idx]) - $
                         sli_cents[i]


        ;; Since I am no longer dividing the image by the
        ;; cross-dispersion spectrum, all columns need to have exactly
        ;; the same number of pixels, or else comparing columns
        ;; (making a sensible spectrum) doesn't work.  That means we
        ;; should do the S/N calculation on norm_xdisp to decide what
        ;; rows to use ahead of time.

        sort_idx = reverse(sort(norm_xdisp/norm_xdisp_err2))
        signal = total(norm_xdisp)
        noise = total(norm_xdisp_err2)
        
        next_sig = signal - norm_xdisp[sort_idx[nxpts-1]]
        next_noise = noise - norm_xdisp_err2[sort_idx[nxpts-1]]
        while nxpts gt 2 and $
          next_sig/next_noise gt signal/noise*sn_imp do begin
           nxpts = nxpts - 1
           signal = next_sig
           noise = next_noise
           next_sig = signal - norm_xdisp[sort_idx[nxpts-1]]
           next_noise = noise - norm_xdisp_err2[sort_idx[nxpts-1]]
        endwhile
        ;; Make a new cross-dispersion index array that has just these
        ;; high S/N points...  Pixel coordinates are relative to
        ;; norm_xdisp, which is a subset of the original image coords
        xdisp_SN_idx = sort_idx[0:nxpts-1]
        ;; and put it in proper sequential order
        sort_idx = sort(xdisp_SN_idx)
        xdisp_SN_idx = xdisp_SN_idx[sort_idx]

        ;; Now collapse the image in the cross dispersion direction to
        ;; get rid of low S/N rows
        collapsed_im = im[*,full_xdisp_good_idx[xdisp_SN_idx]]
        collapsed_err_im2 = err_im2[*,full_xdisp_good_idx[xdisp_SN_idx]]
        if keyword_set(TV) then begin
           display, collapsed_im, hdr, /reuse
        endif

        ;; Create our dispersion axes full length and deal with NANs
        ;; in the fitting software
        pix_axis = indgen(nx)
        wave_axis = fltarr(nx)
        ssg_spec_extract, collapsed_im, hdr, spec, xdisp, /TOTAL
        ssg_spec_extract, collapsed_err_im2, hdr, spec_err2, xdisp_err2, /TOTAL

        ;; Now pick out NAN points, which will mess up curve fitting
        ;; things.  Hey, IDL has a bug where you have to specify
        ;; complement to get ncomplement, otherwise the program crashes!        

        good_idx = where(finite(spec) eq 1 and $
                         finite(spec_err2) eq 1, ngood_disp, $
                         complement=bax_idx, ncomplement=nbad_disp)

        ;; Here is where the reference pixel of the dispersion axis is
        ;; defined to be nx/2., that is the central pixel of the image
        wave_axis = make_disp_axis(dispers, pix_axis, nx/2.)

        ;; Divide by exposure time, get errors out of quadradure mode
        spec = spec/exptime
        spec_err = sqrt(spec_err2)/exptime
        xdisp = xdisp/exptime
        xdisp_err = sqrt(xdisp_err2)/exptime

        ;; Put these into the array that will be loaded into the database
        disp_pix	[0:nx-1, i] = pix_axis
        xdisp_pix	[0:nxpts-1, i] = full_xdisp_good_idx[xdisp_SN_idx]
        wavelengths	[0:nx-1, i] = wave_axis
        spectra		[0:nx-1, i] = spec
        spec_errors	[0:nx-1, i] = spec_err

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
        n_disp       [i] = ngood_disp
        n_xdisp      [i] = nxpts

        if keyword_set(showplots) then begin
           wset,winnum
           ;; This has to be in font !3 for the angstrom symbol to be
           ;; found
           title=string(object, ' ', shortfile, ' ', sxpar(hdr, 'DATE'))
           xtitle='Wavelength ('+string("305B)+')' ;"
           ytitle=string('Signal (', sxpar(hdr, 'BUNIT'), '/S)')
           plot, wave_axis, spec, psym=dot, $
                 title=fname, xtitle=xtitle, ytitle=ytitle, xstyle=1
           oploterr, wave_axis, spec, spec_err, dot
        endif

        ;; For Voigt fit, close up the gaps in all the dispersion
        ;; direction axes.
        if nbad_disp gt 0 then begin
           temp = pix_axis[good_idx] & pix_axis = temp
           temp = wave_axis[good_idx] & wave_axis = temp
           temp = spec[good_idx] & spec = temp
           temp = spec_err[good_idx] & spec_err = temp
        endif
        wave_axis = wave_axis - 6300
        
        temp=strmid(files[i], 0, strpos(files[i], 'r.fits'))
        fname=temp+'.spe'
        message, /INFORMATIONAL, 'Writing ' + fname
        
        openw, lun, fname, /get_lun
        for id = 0,ngood_disp-1 do begin
           printf, lun, wave_axis[id], spec[id], spec_err[id]
        endfor
        close, lun
        free_lun, lun
        ;; Quietly change the mode and group of the file to be group
        ;; lyra writable so other people can work on this
        spawn, string('chmod g+w ', fname), txtout, errout
        spawn, string('chgrp lyra ', fname), txtout, errout

        ngood_files = ngood_files + 1

     endelse ;; CATCH if err
  endfor  ;; All extractions

  CATCH, /CANCEL

  if ngood_files eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  dbclose                       ; just in case
  if NOT keyword_set(noninteractive) then begin
     dbopen, rdbname, 0
     legend = ['Median spectral value', $
               'Average spectral value', $
               'Dist of xdisp COG from slicer center *100']
     marked_ndays = ssg_mark_bad(ndays, [[med_specs], [av_specs], [cross_dcogs*100.]], $
                                 title=string('Continuum levels and bad pixels ', indir), $
                                 xtickunits='Hours', $
                                 xtitle=string('UT time (Hours) ', utdate), $
                                 ytitle='Spectral value (electrons/s)', $
                                 window=7, legend=legend, $
                                 /MJD)
     
     dbclose
     
     bad_idx = where(finite(marked_ndays) eq 0, count)
     if count gt 0 then badarray[bad_idx] = badarray[bad_idx] OR 512
     
     if NOT keyword_set(write) then begin
        answer = ''
        for ki = 0,1000 do flush_input = get_kbrd(0)
        repeat begin
           message, /CONTINUE, 'Write these spectra to the database, erasing previous versions?(Y/[N])'
           answer = get_kbrd(1)
           if byte(answer) eq 10 then answer = 'N'
           answer = strupcase(answer)
        endrep until answer eq 'Y' or answer eq 'N'
        for ki = 0,1000 do flush_input = get_kbrd(0)
        if answer eq 'Y' then write=1
     endif
  endif ;; interactive
  
  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, rdbname, 1
     dbupdate, entries, 'bad, wavelen, spectrum, spec_err, cross_disp, cross_err, disp_pix, xdisp_pix', badarray, wavelengths, spectra, spec_errors, cross_disps, cross_errors, disp_pix, xdisp_pix
     dbupdate, entries, 'med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross, n_disp, n_xdisp, cross_dcog', med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps, n_disp, n_xdisp, cross_dcogs
     dbclose

     ;; Now let's initialize the fitting database
     dbopen, fdbname[0], 0

     ;; Trying to use fancy list-building code
     for i=0, ngood_files-1 do begin
        reinit = where_nday_eq(ndays[i], COUNT=count,SILENT=silent)
        if count gt 1 then begin
           message, /CONTINUE,  'WARNING: more than one entry for this nday was found in the fit database.  Deleting both entries and starting over.'
           if N_elements(delete_list) eq 0 then begin
              delete_list = reinit
           endif else begin
              delete_list = [delete_list, reinit]
           endelse
           count = 0
        endif
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
     if N_elements(build_list) gt 0 then begin
        dbopen, fdbname[0], 1
        ;; This seems to leave the database closed
        dbbuild, build_list, blnew_spec_val
     endif
     if N_elements(reinit_list) gt 0 then begin
        dbopen, fdbname[0], 1
        dbupdate, reinit_list, 'new_spec', new_spec_val
     endif
     ;; Deleting has to be last, since it changes the indices
     if N_elements(delete_list) gt 0 then begin
        dbopen, fdbname[0], 1
        oldpriv=!priv
        !priv = 3
        dbdelete, delete_list
        ;; Latest version of dbdelete does not need dbcompress
        ;;dbcompress, fdbname[0]
        ;; Quietly change the mode and group of the file to be group
        ;; lyra writable so other people can work on this
        dbfname = find_with_def(dbname + '.dbf','ZDBASE')
        spawn, string('chmod g+w ', dbfname), txtout, errout
        spawn, string('chgrp lyra ', dbfname), txtout, errout
        !priv=oldpriv
     endif
     dbclose
     dbopen, fdbname[1], 1
     if N_elements(build_list) gt 0 then begin
         dbbuild, build_list
     endif
     dbclose

     dbopen, rdbname, 0
     rentries = where_nday_eq(good_ndays, COUNT=count,SILENT=silent)
     dbext, rentries, "bad, object, obj_code, wavelen, spectrum, spec_err, cross_disp, cross_err", badarray, objects, obj_codes, wavelengths, spectra, spec_errors, cross_disps, cross_errors
     dbext, rentries, "med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross", med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps
     dbext, rentries, "m_dispers, dispers, n_disp, n_xdisp, cross_dcog", m_disperss, disperss, n_disp, n_xdisp, cross_dcogs
     dbclose

     dbopen, fdbname[0], 1
     fentries = where_nday_eq(good_ndays, SILENT=silent)
     dbupdate, fentries, "bad, object, obj_code, wavelen, spectrum, spec_err, cross_disp, cross_err", badarray, objects, obj_codes, wavelengths, spectra, spec_errors, cross_disps, cross_errors
     dbupdate, fentries, "med_spec, av_spec, min_spec, max_spec, med_cross, av_cross, min_cross, max_cross", med_specs, av_specs, min_specs, max_specs, med_xdisps, av_xdisps, min_xdisps, max_xdisps

     ;; (re)initialize fit_vers
     fit_vers=bytarr(N_elements(fentries))
     dbupdate, fentries, "fit_vers, m_dispers, dispers, n_disp, n_xdisp, cross_dcog", fit_vers, m_disperss, disperss, n_disp, n_xdisp, cross_dcogs

     dbclose

     !priv=oldpriv
     message, /INFORMATIONAL, 'Wrote spectra to and associated info to ' + rdbname + ' and initialized ' + fdbname[0] + ' and ' + fdbname[1] + '.  Run ssg_fit next'
  endif

     ;; For convenience 
     message, /INFORMATIONAL, 'Directory is set to ' + indir

     ;; Clean up after any messes I may have created in processing
     ;; this directory (a bit of a lazy hack)
     close, /all
end

