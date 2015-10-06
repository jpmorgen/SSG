; +
;; $Id: ssg_lparinfo.pro,v 1.2 2015/03/04 15:45:32 jpmorgen Exp $

;; ssg_lparinfo
;;
;; Build the initial line parinfo for ssg observations from line
;; catalogs.  At the moment, this assumes no instrument profile is
;; used in the model calculations, so widths are specific to the
;; echelle.  Other than that, it is pretty general and has a good
;; example of caching code.

;; inwave	center wavelength or a wavelength range
;; wdelta	if center wavelength specified, this is used to 
;; 		calculate the wavelength range
;; indir 	location to find previously written lparinfo files
;;		default: top level of the ssg reduced directory
;; outdir	directory to put the contents of *!ssg.lparinfo
;;
;; You don't have a choice about the filenames.  They are of the form:
;; lparinfo_0000-1111.sav, where 0000 and 1111 are the rounded off
;; string representations of inwave[0] and inwave[1].

; -

pro ssg_lparinfo_finish, inwave=inwave, outdir=outdir, clear_cats=clear_cats
  ;; primitive called just before return to take care of any common
  ;; tasks (yes, I am trying to avoid using gotos!)

  ;; Clear large ASCII catalogs from memory.
  if keyword_set(clear_cats) then $
    lc_clear_cats

  if keyword_set(outdir) then begin
     if NOT ptr_valid(!ssg.lparinfo) then $
       message, 'ERROR: no valid lparinfo available to write to ' + outdir

     lparinfo = *!ssg.lparinfo
     fname = outdir + '/' + 'lparinfo_' + strtrim(round(inwave[0]), 2) + '-' + $
             strtrim(round(inwave[1]), 2) + '.sav'
     save, lparinfo, filename=fname
     message, 'NOTE: Wrote ' + fname, /CONTINUE
  endif ;; write output file

end

pro ssg_lparinfo, inwave, wdelta, indir=indir, outdir=outdir, reread=reread, $
                  rebuild=rebuild, clear_cats=clear_cats, maxdiff=maxdiff, $
                  min_ew

  init = {ssg_sysvar}

  if N_elements(inwave) eq 0 then begin
     inwave = 6300
     message, 'WARNING: no input wavelength specified.  Using ' + strtrim(inwave, 2), /CONTINUE
  endif
  if N_elements(inwave) eq 1 then begin
     if N_elements(wdelta) eq 0 then begin
        wdelta = 50
        message, 'WARNING: no wavelength range specified.  Using  ' + strtrim(wdelta, 2) + ' about ' + strtrim(inwave, 2), /CONTINUE
     endif
     inwave = [inwave - wdelta/2., inwave + wdelta/2.]
  endif

  if NOT ( keyword_set(reread) or keyword_set(rebuild) ) then begin
     ;; Check to see if the current lparinfo list contains our range.
     if ptr_valid(!ssg.lparinfo) then begin
        lc_idx = where((*!ssg.lparinfo).sso.ptype eq !sso.line and $
                      (*!ssg.lparinfo).sso.ttype eq !sso.center)
        if min((*!ssg.lparinfo)[lc_idx].sso.rwl, /NAN) le inwave[0] and $
          inwave[1] le max((*!ssg.lparinfo)[lc_idx].sso.rwl, /NAN) then begin
           ssg_lparinfo_finish, inwave=inwave, outdir=outdir, clear_cats=clear_cats
           return
        endif ;; The lparinfo array in memory is still good.
     endif ;; existing !ssg.lparinfo
     message, 'WARNING: wavelength range not covered in current !ssg.lparinfo.  Searching for/generating another lparinfo list.', /CONTINUE
  endif ;; Not forcing a reread line list

  ;; Unless we want to rebuild, look for an already existing lparinfo
  ;; file.
  if NOT keyword_set(rebuild) then begin
     if NOT keyword_set(indir) then $
       indir = !ssg.top + '/reduced'
     files = file_search(indir + '/lparinfo_*.sav')
     nfiles = N_elements(files)
     if nfiles gt 0 then begin
        wstart = dblarr(nfiles)
        wstop = wstart
        dist = wstart
     endif ;; the for loop is skipped if nfiles eq 0
     for ifi=0, nfiles-1 do begin
        ;; Loop through the files looking for the one with the closest
        ;; match to inwave.  Figure of merit is the sum of the
        ;; distance between the endpoints.
        underscore = strpos(files[ifi], '_');, /reverse_search)
        dash = strpos(files[ifi], '-');, /reverse_search)
        dot = strpos(files[ifi], '.') ;, /reverse_search)
        wstart[ifi] = $
          double(strmid(files[ifi], underscore+1, dash-underscore))
        wstop[ifi] = $
          double(strmid(files[ifi], dash+1, dot-dash))

        dist[ifi] = inwave[0] - wstart[ifi] + wstop[ifi] - inwave[1]
     endfor ;; building list of start and stop wavelengths
     if nfiles gt 0 then begin
        ;; Now look for ones that actually contain our range of
        ;; interest.
        good_idx = where(wstart le inwave[0] and $
                         inwave[1] le wstop, nmatch)
        if nmatch gt 0 then begin
           ;; Pick the first dist ge to 0
           sidx = sort(dist[good_idx])
           ;; unnest
           sidx = good_idx[sidx]
           best_idx = where(dist[sidx] ge 0)
           
           ;; RESTORE lparinfo file and put results into !ssg.lparinfo
           message, 'NOTE: Restoring ' + files[best_idx[0]], /CONTINUE
           restore, files[best_idx[0]], /relaxed_structure_assignment
           !ssg.lparinfo = ptr_new(lparinfo, /no_copy) 

           ssg_lparinfo_finish, inwave=inwave, outdir=outdir, clear_cats=clear_cats
           return

        endif ;; no lparinfo files contain our wavelength range
     endif ;; no files found
  endif ;; not rebuilding

  ;; BUILD LINE LIST and put it into an ssg parinfo array.  Since the
  ;; sun and atmosphere lines have different widths, build them
  ;; separately.
  message, 'NOTE: Building a line list for [' + strtrim(inwave[0],2) + strtrim(inwave[1],2) + ']', /CONTINUE

  if N_elements(maxdiff) eq 0 then begin
     maxdiff = 10
     message, /INFORMATIONAL, 'NOTE: using ' + strtrim(maxdiff, 2) + ' milliAngstroms as the maximum difference between lines in differrent catalogs before they are considered non-matching.'
  endif
  sso = lc_build_sso(wrange=inwave, maxdiff=maxdiff, /milli)
  if NOT keyword_set(clear_cats) then begin
     message, 'WARNING: the line catalogs just read in take a lot of memory.  Unless you are going to repeat this procedure soon, you might want to exit IDL before doing much else or issue the command lc_clear_cats.  This routine also take the switch /lc_clear_cats ', /CONTINUE
  endif

  format = ['f8.3']
  eformat = ['f6.2']

  ;; Sun.  This is mostly Moore with updated wavelengths from
  ;; Pierce-Brekenridge and Allende-Preito, Garcia-Lopez
  par = !values.d_nan
  idx = where(sso.src eq !eph.sun, count)
  if count gt 0 then begin
     par = sso_lc2sso(sso, !pfo.voigt, idx=idx, value=[0,0,150,10], $
                  path=[!eph.sun, !eph.obj, !eph.earth], step=mpstep, $
                  format=format, eformat=eformat, $
                  parinfo_template=!ssg.parinfo, /no_check)
     ;; Set limits on Gausian widths
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.3)
     par[idx].limits = [80,200]
     par[idx].limited = [1,1]
     ;; Set limits on Lorentizan widths
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.4)
     par[idx].limits = [0,100]
     par[idx].limited = [1,1]
     ;; Seems to converge fine when fixed.  Save degrees of freedom this
     ;; way + it goes faster
     idx = where(par.sso.ttype eq !sso.center)
     par[idx].fixed = 1
     lparinfo = array_append(par, lparinfo)
  endif ;; Sun parameters found

  ;; Atmosphere.  This is mostly HITRAN.
  par = !values.d_nan
  idx = where(sso.src eq !eph.earth, count) ;;  !! reusing count
  if count gt 0 then begin
     par = sso_lc2sso(sso, !pfo.voigt, idx=idx, /no_lc_width, $
                      value=[0,0,50,10], $
                      path=[!eph.earth, !eph.earth], step=mpstep, $
                      format=format, eformat=eformat, $
                      parinfo_template=!ssg.parinfo, /no_check)
     ;; Set limits on Gausian widths
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.3)
     par[idx].limits = [20,100]
     par[idx].limited = [1,1]
     ;; Set limits on Lorentizan widths
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.4)
     par[idx].limits = [0,50]
     par[idx].limited = [1,1]
     ;; Seems to converge fine when fixed.  Save degrees of freedom
     ;; this way + it goes faster
     idx = where(par.sso.ttype eq !sso.center)
     par[idx].fixed = 1
     lparinfo = array_append(par, lparinfo)
  endif ;; Atm parameters found     

  ;; With a limited list of wavelengths for airglow and Io, just use
  ;; the system variable line lists.  Tried to put this multi-line
  ;; defining code in sso_fcreate, but it didn't handle value
  ;; correctly.  I had not intended things to be done that way anyway.
  ;; --> not sure if I really meant to start out with a _positive_ value
  par = !values.d_nan
  good_idx = where(inwave[0] le !sso.airglow_lines and $
                   !sso.airglow_lines le inwave[1], nlines)
  for il=0, nlines-1 do begin
     idx = good_idx[il]
     tparinfo = $
       pfo_fcreate(!pfo.sso_funct, /create, ptype=!sso.line, $
                   sso_ftype=!pfo.voigt, path=[!eph.earth, !eph.earth], $
                   rwl=!sso.airglow_lines[idx], format=format, $
                   eformat=eformat, /center, /area, /width, /no_check, $
                   value=[0,1,50,0], parinfo_template=!ssg.parinfo)
     tparinfo[1].parname = 'Airglow'
     par = array_append(tparinfo, par)
  endfor ;; each line
  if nlines gt 0 then begin
     ;; Set limits for the airglow lines similar to the atm absorption
     ;; lines
     idx = where(par.sso.ttype eq !sso.center)
     par[idx].limits = [-4, 4]
     par[idx].limited = [1,1]
     par[idx].fixed = 1
     idx = where(par.sso.ttype eq !sso.ew)
     par[idx].limits[0] = 0
     par[idx].limited[0] = 1
     ;; Set limits on Gausian width
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.3)
     par[idx].limits = [40,100]
     par[idx].limited = [1,1]
     ;; Set limits on Lorentizan widths
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.4)
     par[idx].limits = [0,50]
     par[idx].limited = [1,1]
     lparinfo = array_append(par, lparinfo)
  endif ;; some airglow lines


  ;; Object lines.  This assumes Voigts (not good for Sodium)
  ;; These should be small positive initial value
  par = !values.d_nan
  good_idx = where(inwave[0] le !ssg.obj_lines and $
                   !ssg.obj_lines le inwave[1], nlines)
  for il=0, nlines-1 do begin
     idx = good_idx[il]
     tparinfo = $
       pfo_fcreate(!pfo.sso_funct, /create, ptype=!sso.line, $
                   sso_ftype=!pfo.voigt, path=[!eph.obj, !eph.earth], $
                   rwl=!ssg.obj_lines[idx], format=format, eformat=eformat, $
                   /center, /area, /width, /no_check, $
                   value=[0,1,50,0], parinfo_template=!ssg.parinfo)
     par = array_append(tparinfo, par)
  endfor ;; each line
  if nlines gt 0 then begin
     ;; Fix the wavelength so it doesn't fight the Doppler shift.
     ;; Other limits similar to earth atm lines.
     idx = where(par.sso.ttype eq !sso.center)
     par[idx].fixed = 1
     idx = where(par.sso.ttype eq !sso.ew)
     par[idx].limits[0] = 0
     par[idx].limited[0] = 1
     ;; Set limits on Gausian widths
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.3)
     par[idx].limits = [20,200]
     par[idx].limited = [1,1]
     ;; Set limits on Lorentizan widths
     idx = where(par.sso.pfo.pfo.ftype eq !pfo.voigt + 0.4)
     par[idx].limits = [0,50]
     par[idx].limited = [1,1]
     lparinfo = array_append(par, lparinfo)
  endif ;; some object lines

  if N_elements(lparinfo) eq 0 then $
    message, 'ERROR: no catalog lines found in input wavelength range'

  ;; Remove lines that are too small
  if NOT keyword_set(min_ew) then begin
     min_ew = 1E-6
  endif
  
  ew_idx = where(lparinfo.sso.ptype eq !sso.line and $
                 lparinfo.sso.ttype eq !sso.ew, new)
  if new gt 0 then begin
     ;; I need to separate out the values since the structure confuses
     ;; IDL's where statement
     test_values = lparinfo[ew_idx].value
     small_idx = where(abs(test_values) lt min_ew, nsmall)
     if nsmall gt 0 then begin
        message, /INFORMATIONAL, 'NOTE: deleting ' + strtrim(nsmall, 2) + ' lines because the abs value of their equiv widths are below ' + strtrim(min_ew, 2)
        ;; Make some Doppler in lparinfo groups, so save !sso.dgs in
        ;; case we would mess it up.
        save_sso_dgs = !sso.dgs
        !sso.dgs = ptr_new()
        sso_dg_assign, lparinfo
        for iew=0, nsmall-1 do begin
           idx = ew_idx[small_idx[iew]]
           dg = lparinfo[idx].sso.dg ; Doppler group
           rwl = lparinfo[idx].sso.rwl ; rest wavelength
           myidx = where(lparinfo.sso.dg eq dg and $
                         lparinfo.sso.rwl eq rwl)
           lparinfo[myidx].pfo.status = !pfo.delete
        endfor
     endif ;; found equivalent widths that are too small
     ;; Put back Doppler groups
     sso_dg_assign, /clear
     !sso.dgs = save_sso_dgs
     ;; Delete small lines from lparinfo
     good_idx = where(lparinfo.pfo.status ne !pfo.delete)
     lparinfo = lparinfo[good_idx]
  endif ;; Equivalent width testing


  ;; Make some bogus lines that are marked as not usable but have rwl
  ;; at our end values so we are recognized the next time around.
  end_markers = [lparinfo[0], lparinfo[0]]
  end_markers.fixed = 1
  end_markers.pfo.status = 0
  end_markers.pfo.ftype = 0
  end_markers.sso.ptype = !sso.line
  end_markers.sso.ttype = !sso.center
  end_markers[0].sso.rwl = round(inwave[0])
  end_markers[1].sso.rwl = round(inwave[1])

  lparinfo = [end_markers, lparinfo]

  !ssg.lparinfo = ptr_new(lparinfo, /no_copy) 

  ssg_lparinfo_finish, inwave=inwave, outdir=outdir, clear_cats=clear_cats
  return

end
