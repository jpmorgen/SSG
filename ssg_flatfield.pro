;+
; $Id: ssg_flatfield.pro,v 1.3 2002/12/16 13:41:40 jpmorgen Exp $

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
        asize=size(im) & nx=asize[1] & ny=asize[2]
        flatfile = strtrim(sxpar(hdr,'FLATFILE',COUNT=count))
        if count ne 0 then message, 'ERROR: flat frame ' + flatfile + ' has already been divided'
        biasfile = strtrim(sxpar(hdr,'BIASFILE',COUNT=count))
        if count eq 0 then message, 'ERROR: run ssg_biassub ' + indir

        sxaddhist, string('(ssg_flatfield.pro) ', systime(/UTC), ' UT'), hdr

        ;; Read in bestflat image and line it up
        flat=ssgread(flat_fnames[i], fhdr)
        flat=ssg_flat_align(im, hdr, flat, fhdr)

        ;; Get the flat_cut parameter
        if N_elements(flat_cut) eq 0 then begin
           flat_cut = sxpar(fhdr, 'FLAT_CUT', count=count)
           if count eq 0 then message, 'ERROR: no FLAT_CUT found in flatfield file.  You must therefore specify flat_cut (value below which flatfield image is not divided) on the command line.  0.75 should work OK'
        endif

        ;; Re-normalize flatfield in case using a different flat_cut
        flat = normalize(flat, flat_cut)

        good_idx = where(flat gt flat_cut, count, complement=bad_idx)
        if count eq 0 then message, 'ERROR: no good pixels in flatfield'

        ;; This is the actual flatfielding code.  --> I need to decide
        ;; if I am going to set the pixles outside the flatfield to
        ;; NAN or not.
        im[good_idx] = im[good_idx]/flat[good_idx]
        sxaddhist, "(ssg_flatfield.pro) divided by FLATFILE pixels > FLAT_CUT", hdr
        sxaddpar, hdr, 'FLATFILE', flat_fnames[i], 'Flatfield file'
        sxaddpar, hdr, 'FLAT_CUT', flat_cut, ' cut for flatfield normalization'

        bad_idx = where(flat le flat_cut, count)
        if count gt 0 then begin
           im[bad_idx] = !values.f_nan
           sxaddhist, "(ssg_flatfield.pro) Setting unflattened pixels to NAN", hdr
        endif

        message, /INFORMATIONAL, 'Writing ' + files[i]
        if keyword_set(TV) then display, im, hdr, /reuse
        writefits, files[i], im, hdr

     endelse

  endfor
  CATCH, /CANCEL

end
