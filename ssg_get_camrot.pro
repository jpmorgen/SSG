;+
; $Id: ssg_get_camrot.pro,v 1.1 2002/10/30 15:57:54 jpmorgen Exp $

; ssg_get_camrot.  find the rotation of the camera relative to the
; flatfield pattern

;-

pro ssg_get_camrot, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, pos=pos, nsteps=nsteps, stepsize=stepsize

;  ON_ERROR, 2
  cd, indir
  if NOT keyword_set(nsteps) then nsteps=11 ; 10 steps centered on 0 degrees
  if NOT keyword_set(stepsize) then stepsize=1. ; 1 degree steps in angle
  if NOT keyword_set(order) then order=2

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
  entries = dbfind("typecode>4", $
                   dbfind("bad<16383", $ ; < is really <=
                          dbfind(string("dir=", indir))))
  dbext, entries, "fname, nday, date", files, ndays, dates
  nf = N_elements(files)
  jds = ndays + julday(1,1,1990)
  ;; Use the last file of the day since if you take biases in the
  ;; afternoon, UT date hasn't turned over yet.
  temp=strsplit(dates[nf-1],'T',/extract) 
  utdate=temp[0]
  this_nday = median(ndays)     ; presumably this will throw out anything taken at an odd time
  
  files=strtrim(files)
  correlations = fltarr(nsteps)
  angles = fltarr(nf) 


  if keyword_set(showplots) then window,6

  ngood = 0
  err=0

  for i=0,nf-1 do begin
;     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], hdr) ; Make sure image is in proper orientation
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'ERROR: no BIASFILE keyword in FITS header.  Did you run ssg_biassub yet?'

        ;; Get non-overclock region coordinates
        trimsec = strtrim(sxpar(hdr,'TRIMSEC',COUNT=count))
        if count eq 0 then message, 'ERROR: no TRIMSEC keyword in FITS header'
        toks=strsplit(trimsec,'[:,]')
        if N_elements(toks) ne 4 then message, 'ERROR: unknown TRIMSEC string format'
        coords=strsplit(trimsec,'[:,]',/extract)
        coords = coords - 1     ; Translate into IDL array reference

        ;; Get non-overclock region and mark likely cosmic rays as NAN
        trim_im = im[coords[0]:coords[1],coords[2]:coords[3]]
        badim = mark_cr(trim_im)
        badidx = where(badim eq 1, count)
        if count gt 0 then $
          badim[badidx] = !values.f_nan
        
        trim_im = trim_im + badim

        if keyword_set(TV) then display, trim_im, /reuse
        ;; Collapse spectrum in dispersion and cross-dispersion
        ;; directions to get an image that is guaranteed to be
        ;; oriented perfectly and resembles the orignal image very
        ;; well.
        asize=size(trim_im)
        nx=asize[1]
        ny=asize[2]
        spec = fltarr(nx)
        xdisp = fltarr(ny)
        ;; Start with everything NAN and only put in values if they
        ;; are good.
        spec[*] = !values.f_nan
        xdisp[*] = !values.f_nan
        for si=0,nx-1 do begin
           good_idx = where(finite(trim_im[si,*]), count)
           if count gt 0 then $
             spec[si] = total(trim_im[si,good_idx]) / N_elements(good_idx)
        endfor
        ;; Collapse spectrum in cross-dispersion direction to get spectrum
        for di=0,ny-1 do begin
           good_idx = where(finite(trim_im[*,di]), count)
           if count gt 0 then $
             xdisp[di] = total(trim_im[good_idx,di]) / N_elements(good_idx)
        endfor
        ref_im=template_create(trim_im, spec, xdisp)
        ;; Center of the trimmed image should be close enough to the
        ;; real center of the spectral image so that rotating the
        ;; reference image back and forth doesn't create offset
        ;; problems, which would skew the results.
        for ri=0,nsteps-1 do begin
           angle=stepsize*(ri-nsteps/2.)
           rot_im=rot(ref_im, angle) 
           correlations[ri] = total(trim_im*rot_im,/NAN)
        endfor
        xaxis = stepsize*indgen(nsteps) - stepsize*nsteps/2.
        weights = xaxis
        weights[*] = 1.0
        fit = gaussfit(xaxis, correlations, params)

        if keyword_set(showplots) then begin
           wset,6
           plot, xaxis, correlations, psym=asterisk, title=files[i]
           oplot, xaxis, fit, linestyle=solid
        endif
        angles[i] = params[1]
        ngood = ngood + 1
     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  dbclose

  if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'

  window,7
  plot, ndays, angles, $
        title=string('Camera rotation angles in ', indir), $
        xtickunits='Hours', $
        yrange=[min([angles,angles], /NAN), $
                max([angles, angles], /NAN)], $
        xstyle=1, ystyle=1, psym=plus, $
        xtitle=string('UT time (Hours) ', utdate), $
        ytitle='Angle (degrees)'

  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'cam_rot', angles
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated camera rotation in ' + dbname

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end
