;+
; $Id: ssg_flatfield.pro,v 1.2 2002/11/21 20:04:02 jpmorgen Exp $

; ssg_flatfield Divide comp and object images by flatfield, recording
; flatfield name in database

;-

pro ssg_flatfield, indir, flatname=flatname, flat_cut=flat_cut, tv=tv

  ON_ERROR, 2
  cd, indir

  silent = 1
  if keyword_set(verbose) then silent = 0

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  if keyword_set(flatname) then begin
     oldpriv=!priv
     !priv = 2
     dbopen, dbname, 1
     
     entries = dbfind("typecode=[2,5]", dbfind(string("dir=", indir)))
     nf = N_elements(entries)
     flat_fnames = strarr(nf)
     flat_fnames[*] = flatname
     dbupdate, entries, 'flat_fname', flat_fnames
     dbclose
     !priv=oldpriv
     message, /INFORMATIONAL, 'Updated flat_fnames in ' + dbname
  endif

  dbopen, dbname, 0
  entries = dbfind("typecode=[2,5]", $
                   dbfind("bad<16383", $ ; < is really <=
                          dbfind(string("dir=", indir))))
  dbext, entries, "fname, cam_rot, flat_fname, flat_cut", files, cam_rots, flat_fnames, flat_cuts
  dbclose

  files=strtrim(files)
  flat_fnames=strtrim(flat_fnames)
  nf = N_elements(files)

  err = 0
  for i=0,nf-1 do begin
     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        im = ssgread(files[i], hdr)
        flatfile = strtrim(sxpar(hdr,'FLATFILE',COUNT=count))
        if count ne 0 then message, 'ERROR: flat frame ' + flatfile + ' has already been divided'
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'ERROR: run ssg_biassub ' + indir

        sxaddhist, string('(ssg_flatfield.pro) ', systime(/UTC), ' UT'), hdr

        ;; Read in bestflat image
        flat=ssgread(flat_fnames[i], fhdr)
        if N_elements(flat) ne N_elements(im) then $
          message, 'ERROR: "' + flat_fnames[i] + '" and "' + files[i] + '" are not the same size.  Did you specify flatfile= on the command line?'

        ;; Rotate flatfield image to fit nicely on image we are flattening 
        flat_rot = sxpar(fhdr, 'CAM_ROT', count=count)
        if count eq 0 then message, 'ERROR: flatfield file '+ flat_fnames[i] + ' does not have a CAM_ROT keyword.  Did you use ssg_flatgen to create it?'
        cam_rot = sxpar(hdr, 'CAM_ROT', count=count)
        if count eq 0 then message, 'ERROR: file '+ files[i] + ' does not have a CAM_ROT keyword.  You need to run ssg_[get&fit]_camrot stuff first'
        if flat_rot ne cam_rot then begin
           flat=rot(flat, cam_rot-flat_rot, cubic=-0.5)
           message, /INFORMATIONAL, 'Rotating '+ flat_fnames[i] + ' by ' + string(cam_rot-flat_rot) + ' to line up with ' + files[i]
           sxaddhist, string(format='("(ssg_flatfield.pro) rotated FLATFILE by ", f6.2, " degrees")', rot-flat_rot), hdr
        endif

        ;; Get the flat_cut parameter
        if N_elements(flat_cut) eq 0 then begin
           flat_cut = sxpar(fhdr, 'FLAT_CUT', count=count)
           if count eq 0 then message, 'ERROR: no FLAT_CUT found in flatfield file.  You must therefore specify flat_cut (value below which flatfield image is not divided) on the command line.  0.75 should work OK'
        endif

        flat = normalize(flat, flat_cut)
        ;; Re-normalize flatfield in case using a different flat_cut
        good_idx = where(flat gt flat_cut, count)
        if count eq 0 then message, 'ERROR: no good pixels in flatfield'

        im[good_idx] = im[good_idx]/flat[good_idx]
        sxaddhist, "(ssg_flatfield.pro) divided by FLATFILE pixels > FLAT_CUT", hdr
        sxaddpar, hdr, 'FLATFILE', flat_fnames[i], 'Flatfield file'
        sxaddpar, hdr, 'FLAT_CUT', flat_cut, ' cut for flatfield normalization'

;         bad_idx = where(flat le flat_cut, count)
;         if count gt 0 then begin
;            im[bad_idx] = !values.f_nan
;            sxaddhist, "(ssg_flatfield.pro) Setting unflattened pixels to NAN", hdr
;         endif

        message, /INFORMATIONAL, 'Writing ' + files[i]
        if keyword_set(TV) then display, im, hdr, /reuse
        writefits, files[i], im, hdr

     endelse

  endfor
  CATCH, /CANCEL

end
