;+
; $Id: ssg_lightsub.pro,v 1.1 2003/03/10 18:32:12 jpmorgen Exp $
;-

;; ssg_lightsub Calculates the background spectrum from room lights or
;; whatever, for each image using the rows above and below the slicer
;; image.  Stores spectra in database and subtracts them off of each image

pro ssg_lightsub, indir, VERBOSE=verbose, showplots=showplots, TV=tv, zoom=zoom, pos=pos
  ON_ERROR, 0
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode>1", $
                   dbfind("bad<8191", $ ; < is really <=
                          dbfind(string("dir=", indir))))
  dbext, entries, 'fname, m_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, m_cam_rot, cam_rot', $
         files, m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, m_cam_rots, cam_rots
  dbext, entries, 'back_light, med_back, av_back, stdev_back', $
         back_lights, med_back, av_back, stdev_back

  dbclose

  files=strtrim(files)
  nf = N_elements(files)
  sli_bots = floor(sli_bots)
  sli_tops = ceil(sli_tops)

  err = 0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; Read in the data section for calculations but the full
        ;; section for rewriting (see below)
        im = ssgread(files[i], hdr, eim, ehdr, /DATA, /TRIM)
        bim = ssgread(files[i], hdr, beim)
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'ERROR: you must run ssg_biassub first'

        asize = size(im) & nx = asize(1) & ny = asize(2)

        rot_im = ssg_camrot(im, -cam_rots[i], nx/2., sli_cents[i])
        edge_im = fltarr(nx,ny-(sli_tops[i]-sli_bots[i]))
        edge_im[*,0:sli_bots[i]-1] = rot_im[*,0:sli_bots[i]-1]
        edge_im[*,sli_bots[i]:ny-(sli_tops[i]-sli_bots[i])-1] = $
          rot_im[*,sli_tops[i]:ny-1]

        if keyword_set(TV) then $
          display, edge_im, /reuse

        edge_spec = fltarr(nx)
        edge_err = edge_spec
        for ix=0,nx-1 do begin
           ;; Calculate median only, since there is certain to be
           ;; contamination from the wing of the light coming through
           ;; the slicer/exit slit jaws that would mess up the average
           edge_spec[ix] = median(edge_im[ix,*])
           ;; This is almost certainly an overestimate of the error.
           ;; Indeed, when there is a cosmic ray hit, it goes wild.
           edge_err[ix] = stddev(edge_im[ix,*], /NAN)
        endfor
        ;; So, lets median filter the error spectrum
        edge_err = median(edge_err, 4)

        junk = strtrim(sxpar(hdr,'MEDBACK',COUNT=count))
        if count ne 0 then message, 'ERROR: background light has already been subtracted.  Database value for median of the background light is ' + string(med_back[i]) + '.  Header has value of ' + string(junk)
        template = template_create(im, edge_spec)
        err_temp = template_create(im, edge_err)

        ;; We want to subtract the background from everything except
        ;; the overclock region
        sxaddhist, string('(ssg_lightsub.pro) ', systime(/UTC), ' UT '), hdr
        sxaddhist, string('(ssg_lightsub.pro) Subtracted background light spectrum measured from'), hdr
        sxaddhist, string('(ssg_lightsub.pro) edge of image.  Overclock region not touched'), hdr
        im = im - template
        eim = sqrt(eim^2 + err_temp^2)

        bim[0:nx-1,0:ny-1] = im[*,*]
        beim[0:nx-1,0:ny-1] = eim[*,*]

        back_lights[*,i]= edge_spec[*]
        med_back[i] 	= median(edge_spec)
        av_back[i] 	= mean(edge_spec, /NAN)
        stdev_back[i] 	= stddev(edge_spec, /NAN)

        sxaddpar, hdr, 'MEDBACK', med_back[i], 'Median value of subtracted background light (BUNIT)'
        sxaddpar, hdr, 'AVBACK', av_back[i], 'Average value of subtracted background light (BUNIT)'
        sxaddpar, hdr, 'STDBACK', stdev_back[i], 'Stdev value of subtracted background light (BUNIT)'

        ssgwrite, files[i], bim, hdr, beim, ehdr
     endelse
     
  endfor
  CATCH, /CANCEL

  if NOT keyword_set(write) then begin
     for ki = 0,1000 do flush_input = get_kbrd(0)
     repeat begin
        message, /CONTINUE, 'Write these values to the database?([Y]/N)'
        answer = get_kbrd(1)
        if byte(answer) eq 10 then answer = 'Y'
        answer = strupcase(answer)
     endrep until answer eq 'Y' or answer eq 'N'
     for ki = 0,1000 do flush_input = get_kbrd(0)
     if answer eq 'Y' then write=1
  endif
  
  if keyword_set(write) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     dbupdate, entries, 'back_light, med_back, av_back, stdev_back', $
               back_lights, med_back, av_back, stdev_back
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated background light stuff in ' + dbname
  endif ;; write

  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir
end
