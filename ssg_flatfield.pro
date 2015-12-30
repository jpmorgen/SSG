;+
; $Id: ssg_flatfield.pro,v 1.6 2003/06/13 03:51:53 jpmorgen Exp $

; ssg_flatfield Divide comp and object images by flatfield, recording
; flatfield name in database.  If specified, also divide by the sky
; slicer flat.

;-

pro ssg_flatfield, indir, lampflat_dir=lampflat_dir, skyflat_dir=skyflat_dir, flat_cut=flat_cut, sky_cut=sky_cut, tv=tv

  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  entries = dbfind("typecode=[2,5]", $
                   dbfind("bad<16383", $ ; < is really <=
                          dbfind(string("dir=", indir), /fullstring)))
        dbext, entries, 'fname, bad, typecode, lampflat_dir, skyflat_dir, flat_cut, sky_cut', $
         files, badarray, typecodes, lampflat_dirs, skyflat_dirs, flat_cuts, sky_cuts
        dbext, entries, 'm_sli_bot, e_sli_bot, sli_bot, m_sli_top, e_sli_top, sli_top, sli_cent, e_sli_cent, m_cam_rot, cam_rot', $
         m_sli_bots, e_sli_bots, sli_bots, m_sli_tops, e_sli_tops, sli_tops, sli_cents, e_sli_cents, m_cam_rots, cam_rots

  dbclose

  files=strtrim(files)
  nf = N_elements(files)
  if keyword_set(lampflat_dir) then lampflat_dirs[*]=lampflat_dirs
  lampflat_dirs=strtrim(lampflat_dirs)
  skyflat_dirs=strtrim(skyflat_dirs)

  err = 0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], hdr, eim, ehdr)
        asize=size(im) & nx=asize[1] & ny=asize[2]
        flatfile = strtrim(sxpar(hdr,'LAMPDIR',COUNT=count))
        if count ne 0 then message, 'ERROR: flats from ' + flatfile + ' have already been divided'
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'ERROR: run ssg_biassub ' + indir

        sxaddhist, string('(ssg_flatfield.pro) ', systime(/UTC), ' UT'), hdr

        if keyword_set(lampflat_dir) then $
          lampflat_dirs[i] = lampflat_dir
        if strlen(lampflat_dirs[i]) eq 0 then $
          lampflat_dirs[i] = indir
        if keyword_set(skyflat_dir) then $
          skyflat_dirs[i] = skyflat_dir
        if strlen(skyflat_dirs[i]) eq 0 then $
          skyflat_dirs[i] = 'NONE'

        if keyword_set(flat_cut) then $
          flat_cuts[i] = flat_cut
        if keyword_set(sky_cut) then $
          sky_cuts[i] = sky_cuts

        sxaddpar, hdr, 'LAMPDIR', lampflat_dirs[i], 'Dir with reduced lamp flats'
        sxaddpar, hdr, 'SKYDIR', skyflat_dirs[i], 'Dir with reduced sky flats'

        type = 'lamp'
        repeat begin
           ;; Lamp, sky loop
           subtype = 'slicer'
           repeat begin
              ;; slicer, dust, source loop
              flat_name = string(type+'_'+subtype+'_flat.fits')
              dir = lampflat_dirs[i]
              if type eq 'sky' then $
                dir = skyflat_dirs[i]
              flat = ssgread(string(dir, '/', flat_name), fhdr, feim, fehdr)

              ;; Get the flat_cut parameter
              if type eq 'lamp' then begin
                 if keyword_set(flat_cut) then begin
                    ff_flat_cut = flat_cut
                 endif else begin
                    ff_flat_cut = sxpar(fhdr, 'FLAT_CUT', count=count)
                    if count eq 0 then message, 'ERROR: no FLAT_CUT found in flatfield file.  You must therefore specify flat_cut (value below which flatfield image is not divided) on the command line.'
                 endelse

                 flat_cuts[i] = ff_flat_cut
                 sxaddpar, hdr, 'FLAT_CUT', flat_cuts[i], ' cut for flatfield normalization'
                 ;; The current versions of the IDL FITS stuff trims
                 ;; keywords to 8 characters
              endif ;; lamp flat

              ;; Get the sky_cut parameter
              if type eq 'sky' then begin
                 if keyword_set(sky_cut) then begin
                    ff_flat_cut = sky_cut
                 endif else begin
                    ff_flat_cut = sxpar(fhdr, 'SKY_CUT', count=count)
                    if count eq 0 then message, 'ERROR: no SKY_CUT found in flatfield file.  You must therefore specify sky_cut (value below which flatfield image is not divided) on the command line.'
                 endelse

                 sky_cuts[i] = ff_flat_cut
                 sxaddpar, hdr, 'SKY_CUT', sky_cuts[i], ' cut for skyflat normalization'
              endif  ;; sky flat


              ;; If we have a sky flat, we don't want to divide the
              ;; lamp slicer flat
              if skyflat_dirs[i] ne 'NONE' and $
                type eq 'lamp' and subtype eq 'slicer' then begin
                 sxaddhist, '(ssg_flatfield.pro) SKYSLICE detected, not dividing by LAMPSLIC ', hdr
              endif else begin

                 ;; Only the dust flat can be divided directly into the
                 ;; image.  All the others will have a little shifting
                 ;; and rotating to do
                 if subtype ne 'dust' then begin
                    flat=ssg_flat_align(im, hdr, flat, fhdr)
                 endif

                 ;; Normalize flatfield to the flat_cut value
                 flat = normalize(flat, ff_flat_cut, factor=factor)
                 feim = feim*factor

                 ;; Decide which flatfield pixels to divide.  We want
                 ;; to avoid pixels less than the flat_cut, except for
                 ;; pixels = 0, which will end up being NAN in the
                 ;; final image (this is how deep dust spots are measured)
                 good_idx = where(flat gt ff_flat_cut or flat eq 0, count)

                 if count eq 0 then message, 'ERROR: no good pixels in flatfield'

                 ;; This is the actual flatfielding code.  Do the
                 ;; division only for good pixels + use the slicer flat
                 ;; later to mask out the edges.
                 oim = im
                 im[good_idx] = im[good_idx]/flat[good_idx]
                 ;; The flatfield images are the total electrons in
                 ;; each pixel, so Poisson statistics apply.
                 eim[good_idx] = im[good_idx] * $
                                 sqrt( (eim[good_idx] / oim[good_idx])^2 + $
                                       (feim[good_idx] / flat[good_idx])^2 )

                 ;; Inf is a possible answer, which is x/0 and
                 ;; distinct from NAN.  So replace Inf with NAN
                 bad_idx = where(finite(im) eq 0, count)
                 if count gt 0 then $
                   im[bad_idx] = !values.f_nan
                 bad_idx = where(finite(eim) eq 0, count)
                 if count gt 0 then $
                   eim[bad_idx] = !values.f_nan


                 keyword = strmid(strupcase(type+subtype), 0,8)
                 sxaddhist, '(ssg_flatfield.pro) divided by ' + keyword, hdr
                 sxaddpar, hdr, keyword, flat_name, string(type + ' ' + subtype + ' flat in ' + strupcase(type) + 'DIR')

                 message, /INFORMATIONAL, 'Writing ' + files[i]
                 if keyword_set(TV) then begin
                    crx=1
                    if typecodes[i] eq 2 then crx=0
                    display, im, hdr, /reuse, crx=crx
                 endif
                 ssgwrite, files[i], im, hdr, eim, ehdr
              endelse ;; OK to divide flatfield

              case subtype of
                 'slicer': begin
                    subtype = 'dust'
                 end
                 'dust': begin
                    subtype = 'source'
                 end
                 'source': begin
                    subtype = 'DONE'
                 end
              endcase
           endrep until subtype eq 'DONE'
              
           if type eq 'sky' then $
             type = 'DONE'
           if type eq 'lamp' then $
             type = 'sky'
           if skyflat_dirs[i] eq 'NONE' then $
             type = 'DONE'
        endrep until type eq 'DONE'

     endelse ;; No error

     CATCH, /CANCEL
  endfor

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'lampflat_dir, skyflat_dir, flat_cut, sky_cut', $
            lampflat_dirs, skyflat_dirs, flat_cuts, sky_cuts
  
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated flatfield info in ' + dbname


end
