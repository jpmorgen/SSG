;+
; $Id: ssg_get_dispers.pro,v 1.1 2002/11/25 19:02:52 jpmorgen Exp $

; ssg_get_dispers.  Use comp lamp spectra to find dispersion relation

;-

function delta_spec, X, params, N_continuum=N_continuum

  n_params = N_elements(params)
  Yaxis = fltarr(N_elements(X))
  if N_elements(params) eq 0 then return, Yaxis

  dps = params                  ; delta function paramters
  if keyword_set(N_continuum) then begin
     if n_params lt N_continuum then message, 'ERROR: not enough parameters specified (' + string(n_params) + ') for N_continuum = ' + string(N_continuum)
     for n=0,N_continuum-1 do begin
        Yaxis = Yaxis + params[n]*X^n
     endfor
     dps = params[N_continuum:n_params-1]
  endif

  ;; Collect parameters into form that deltafn can use
  if N_elements(dps) mod 2 ne 0 then message, 'ERROR: wrong number of parameters.  Must have N_continuum continum parameters (polynomial assumed) and an even number of deltafuction parameters after that (X,Y)'
  nlines = N_elements(dps)/2
  Xs = fltarr(nlines)
  Yvals = Xs
  for i=0, nlines-1 do begin
     Xs[i] = dps[2*i]
     Yvals[i] = dps[2*i+1]
  endfor

  return, deltafn(Xs, Yvals, Yaxis)
end

function voigt_spec, X, params, dparams, N_continuum=N_continuum, Vaxis=Vaxis

  n_params = N_elements(params)
  Yaxis = fltarr(N_elements(X))
  if N_elements(params) eq 0 then return, Yaxis

  vps = params                  ; voigt parameters
  if keyword_set(N_continuum) then begin
     if n_params lt N_continuum then message, 'ERROR: not enough parameters specified (' + string(n_params) + ') for N_continuum = ' + string(N_continuum)
     for n=0,N_continuum-1 do begin
        Yaxis = Yaxis + params[n]*X^n
     endfor
     vps = params[N_continuum:n_params-1]
  endif

  ;; Collect parameters into form that deltafn can use
  if N_elements(vps) mod 4 ne 0 then message, 'ERROR: wrong number of parameters.  Must have N_continuum continum parameters (polynomial assumed) and four parameters per Voigt profile'

  return, voigtfn(vps, X, Yaxis)
end

;; Make a potentially sparse axis using a vector dispersion
;; descirption [ref wavelength, higher order dispersion terms]
function make_disp_axis, disp, in_axis, ref_pixel
  axis = in_axis
  axis = axis * 0.
  order = N_elements(disp)-1
  for di = 0,order do begin
     axis = axis + disp[di]*(in_axis-ref_pixel)^di
  endfor
  return, axis
end

function align_disp, in_disp, line, pixel, ref_pixel
  disp=in_disp
  order = N_elements(disp)-1
  old_val = 0
  for di = 0,order do begin
     old_val = old_val + disp[di]*(pixel-ref_pixel)^di
  endfor
  disp[0] = disp[0] - old_val + line
  return, disp
end

;; make associations between lists.  list2, or the longer list, is
;; assumed to be the master list.  The indecies of list1 that reflect
;; the best matches to list2 are returned.  min_diff is the
;; cummulative difference between the lists in the best case match
function list_associate, list, master_list, min_diff=min_diff
  nl = N_elements(list)
  nml = N_elements(master_list)
  if nl gt nml then $
    message, 'ERROR: master list must be the same size or greter than list'
  associations = intarr(nl)

  ;; Make an array that has all the differences between the elements
  ;; of two lists
  diffarray = fltarr(nl, nml)
  for i = 0,nl-1 do begin
     diffarray[i,*] = abs(master_list[*] - list[i])
  endfor

  ;; Now systematically match our lits using the minima in diffarray.
  ;; The mapping must be one to one, so blank out the parts of the
  ;; array that we have already used.  In order to avoid false matches
  ;; by a change close association early in the list, loop through all
  ;; possible combinations and take the one that minimizes the total
  ;; difference.

  min_diffs = fltarr(nl)
  for outer_loop = 0, nl-1 do begin
     ;; Start by finding the closest match to the current outer_loop
     ;; list element
     da = diffarray             ; Start with a fresh diffarray each time
     min_diffs[outer_loop] = min(da[outer_loop, *], iml)
     da[outer_loop, *] = !values.f_nan
     da[*, iml] = !values.f_nan
     ;; Now let the rest fall into place for this iteration
     for mdi = 1, nl-1 do begin
        min_diffs[outer_loop] = min_diffs[outer_loop] + min(da, min_idx, /NAN)
        ;; Unwrap 1D array index into 2D version so we can blank out
        ;; used part of da
        il = min_idx[0] mod nl
        iml = fix(min_idx[0]/nl)
        da[il, *] = !values.f_nan
        da[*, iml] = !values.f_nan
     endfor
  endfor
  ;; Now see which iteration gave the best overall answer and use it
  ;; to generate the final association list
  min_diff = min(min_diffs, outer_loop)
  da = diffarray
  min_diff = min(da[outer_loop, *], iml)
  associations[il] = iml
  da[il, *] = !values.f_nan
  da[*, iml] = !values.f_nan
  ;; Now let the rest fall into place
  for mdi = 1, nl-1 do begin
     min_diff = min_diff + min(da, min_idx, /NAN)
     il = min_idx[0] mod nl
     iml = fix(min_idx[0]/nl)
     associations[il] = iml
     da[il, *] = !values.f_nan
     da[*, iml] = !values.f_nan
  endfor

  return, associations

end

function line_correlate, in_disp, no_dp, line_pix=line_pix, line_list=line_list, line_stengths=line_strengths, npts=npts, bad_fraction=bad_fraction

;   wspan = in_disp[1] * npts
;   wmin = in_disp[0] - wspan/2.
;   wmax = wmin + wspan
;   atlas_idx = where(line_list ge wmin and line_list lt wmax, $
;                     n_expected_lines)
;   if n_expected_lines eq 0 then return, !values.f_nan
  
;  plot,line_pix, line_list[atlas_idx], xstyle=2, ystyle=2, $
;       yrange=[min(line_list[atlas_idx]),max(line_list[atlas_idx])], $
;       psym=asterisk
  

  ;; Make predicted line list and match lines to those in the atlas
  ;; line list (line_list)
  pred_lines = make_disp_axis(in_disp, line_pix, npts/2.)
  n_fit_lines = N_elements(pred_lines)
  n_line_list = N_elements(line_list)
  diffarray = dblarr(n_fit_lines, n_line_list)
  for i=0,n_fit_lines-1 do begin
     diffarray[i,*] = abs(line_list[*] - pred_lines[i])
  endfor
  min_dists = dblarr(n_fit_lines)
  for i=0,n_fit_lines-1 do begin
      min_dists[i] = min(diffarray, min_idx, /NAN)
      ;; Unwrap the index to get a 2D coordinate again.
      ifit_line = min_idx[0] mod n_fit_lines
      iline_list = fix(min_idx[0]/n_fit_lines)
      diffarray[ifit_line, *] = !values.f_nan
      diffarray[*, iline_list] = !values.f_nan
   endfor
   ;; Inevitable there will be some line misidentifications.  That
   ;; means the last few distances will be bad.  Let's hack of 10% for
   ;; good measure
;   if NOT keyword_set(bad_fraction) then bad_fraction = .5
;   temp = min_dists[0:fix((1-bad_fraction)*n_fit_lines)]
;   min_dists= temp
   
   ;; Debugging
   ;plot, min_dists

;   bad_idx = where(min_dists gt max(pred_lines) - min(pred_lines), count)
;   if count gt 0 then $
;     min_dists[n_fit_lines-count-1:n_fit_lines-1] = 0

   return, total(min_dists^2)
end


function comp_correlate, in_disp, no_dp, spec=spec, line_list=line_list, line_stengths=line_strengths

  ;; Make this function usable in a variety of contexts later
  if n_params() eq 2 then begin
     if N_elements(in_disp) eq N_elements(no_dp) then $
       message, 'ERROR: I think you are asking me to calculate a derivative of the parameters for tnmin.  I don''nt know how to do this.  Make sure you specify /AUTODERIVATIVE with tnmin'
  endif

  if NOT keyword_set(spec) then message, 'ERROR: I need a spectrum to compare things with'
  if NOT keyword_set(line_list) then message, 'ERROR: I need a line list to compare things with'

  num_lines = N_elements(line_list)
  if NOT keyword_set(line_strengths) then begin
     line_strengths = spec
  endif 


  npts = N_elements(spec)
  disp_axis = dblarr(npts)
  pix_axis = indgen(npts) - npts/2
  order = N_elements(in_disp)-1
  for di = 0,order do begin
     disp_axis = disp_axis + in_disp[di]*pix_axis^di
  endfor
  
  y=fltarr(npts)
  y = deltafn(line_list, line_strengths, y, Xaxis = disp_axis)


;   print, in_disp
;   print,minmax(disp_axis)
;   print,minmax(y)
;   print,minmax(spec)
  
  plot,disp_axis, y
  oplot,disp_axis, spec, linestyle=2
;  wait, 0.5
  return, total(y*spec,/NAN)

end


pro ssg_get_dispers, indir, VERBOSE=verbose, showplots=showplots, TV=tv, atlas=atlas, dispers=in_disp, order=order, N_continuum=N_continuum

;  ON_ERROR, 2
  cd, indir

  if NOT keyword_set(atlas) then atlas='/home/jpmorgen/data/ssg/reduced/thar_list'

  ;; Be careful with type conversion so everything ends up double
  if NOT keyword_set(in_disp) then in_disp=[6300, 0.055,0]
  order = N_elements(in_disp) - 1
  if order lt 0 then order=0
  dispers = dblarr(order+1)
  if NOT keyword_set(N_continuum) then N_continuum = 1

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

  ;; Read in whole atlas
  openr, lun, /get_lun, atlas
  num_lines=0
  ;; Count number of lines.  I thought there was an easy way to do
  ;; this, but couldn't find it in a hurry
  while NOT EOF(lun) do begin
     readf, lun, junk
     num_lines = num_lines + 1
  endwhile
  close, lun
  num_lines = num_lines - 1
  ;; Now really read in atlas
  line_list = dblarr(num_lines)
  openr, lun, /get_lun, atlas
  for li = 0,num_lines - 1 do begin
     readf, lun, temp
     line_list[li]=temp
  endfor
  close, lun
  ;; If we ever get some line strengths this will help, but I think
  ;; that with step-by-step correlation, we can work around that
  line_strengths = fltarr(num_lines)
  line_strengths[*] = 1

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0
  ;; Get all the files in the directory so we can mark camrot as not
  ;; measured on the ones where we can't measure it.
  entries = dbfind("typecode=2", $
                   dbfind("bad<1023", $ ; < is really <=
                          dbfind(string("dir=", indir))))

  ;; TEMPORARY USE OF CROSS_DISP IN DATABSE
  dbext, entries, "fname, nday, cross_disp", $
         files, ndays, disp_arrays

  nf = N_elements(files)
  
  files=strtrim(files)

  if keyword_set(showplots) then window,6

  ngood = 0
  err=0

  for i=0,nf-1 do begin

     message, 'Looking at ' + files[i], /CONTINUE
;     CATCH, err
     if err ne 0 then begin
        message, /NONAME, !error_state.msg, /CONTINUE
        message, 'skipping ' + files[i], /CONTINUE
     endif else begin
        ;; NEED TO FIX THIS UP TOO
        temp=strsplit(files[i], '.fits', /extract)
        files[i]=temp[0]+'.red.fits'

        im = ssgread(files[i], hdr, /DATA)

        ssg_spec_extract, im, hdr, spec, xdisp, /TOTAL

        npts = N_elements(spec)
        pix_axis = indgen(npts) - npts/2
        model_spec=dblarr(npts)
        temp_axis = model_spec
        old_red_chisq = 1

        ;; start each file afresh
        dispers[*] = in_disp[*]

        ;; Now choose our likely window of lines
        wspan = dispers[1] * npts
        wmin = dispers[0] - wspan/2.
        wmax = wmin + wspan
        atlas_idx = where(line_list ge wmin and line_list lt wmax, $
                          n_expected_lines)
        ;; Let's cut down on the number of expected lines so we only
        ;; stick to the brightest
        n_expected_lines = n_expected_lines
        if n_expected_lines lt order + 1 then $
          message, 'ERROR: cannot find enough lines in the atlas to constrain a ' + string(order) + ' order dispersion solotion'
        


;         for di = 0,order do begin
;            temp_axis = temp_axis + in_disp[di]*pix_axis^di
;         endfor
; 
;         temp=deltafn(line_list, line_strengths, model_spec, Xaxis=temp_axis)
;         temp = where(temp gt 0, count)
;         n_expected_lines = count;/2.


        n_params = 0

        ;; Fit delta functions to the comp spectrum
        repeat begin
           residual = spec - model_spec
           next_max = max(residual, next_maxx, /NAN)
           if n_params eq 0 then begin
              params = dblarr(N_continuum + 2)
              params[N_continuum] = next_maxx[0]
              params[N_continuum+1] = next_max[0]
           endif else begin
              params = [params, next_maxx[0], next_max[0]]
           endelse
           to_pass = { N_continuum:N_continuum }
           params = mpfitfun('delta_spec', pix_axis, spec, sqrt(spec), $
                             params, FUNCTARGS=to_pass)
           n_params = N_elements(params)
           model_spec = delta_spec(pix_axis, params, N_continuum=N_continuum)
           red_chisq = total((spec[*] - model_spec[*])^2)/n_params
           nlines = (n_params-N_continuum)/2
        endrep until old_red_chisq ge red_chisq or $
           nlines eq n_expected_lines

        
        window,2
        !p.multi = [0,0,2]
        plot, pix_axis, spec, $
              title=string("Spectrum of comp ", files[i]), $
              xtitle='Pixels ref to center of image', $
              ytitle=string(sxpar(hdr, 'BUNIT'), 'Solid=data, dotted=model')
        oplot, pix_axis, model_spec, linestyle=dotted
        plot, pix_axis, residual, $
              title=string("Fit residual "), $
              xtitle='Pixels ref to center of image', $
              ytitle=string(sxpar(hdr, 'BUNIT'))
        
        !p.multi = 0

        if nlines ne n_expected_lines then $
          message, 'Unsure how to proceed'


        ;; Extract line pixel values 
        dps = params[N_continuum:n_params-1]
        Xs = fltarr(nlines)
        Yvals = Xs
        Yaxis = dblarr(npts)
        for li=0, nlines-1 do begin
           Xs[li] = dps[2*li]
           Yvals[li] = dps[2*li+1]
        endfor

        line_sort=sort(Xs)

        ;; tnmin is having a hard time finding the best fit
        ;; spontaneously, so go through each line and see how things
        ;; look when we line up on it.  This amounts to a preliminary
        ;; grid search on the reference wavelength

        first_pass = fltarr(nlines, n_expected_lines)
        tdisp = dispers
        for icomp=0,nlines-1 do begin
           for iatlas=0,n_expected_lines-1 do begin
              tdisp = align_disp(dispers, line_list[atlas_idx[iatlas]], $
                                 Xs[icomp], npts/2.)
              first_pass[icomp, iatlas] = $
                line_correlate(tdisp, line_pix=Xs, $
                               line_list=line_list[atlas_idx], $
                               npts=npts)
              print, icomp, iatlas, first_pass[icomp, iatlas]
           endfor
        endfor
        temp = min(first_pass, min_idx, /NAN)
        ;; Unwrap the index to get a 2D coordinate again.
        ifit_line = min_idx[0] mod nlines
        iline_list = fix(min_idx[0]/n_expected_lines)

        ;; Initialize the dispersion on our best first guess
        dispers = align_disp(dispers, line_list[atlas_idx[iline_list]], $
                             Xs[ifit_line], npts/2.)

        ;; For display purposes (and maybe fitting later), let's see
        ;; if we can't associate the comp lines to the atlas at this point

        close_match = make_disp_axis(dispers, Xs, npts/2.)
        associations = list_associate(close_match, line_list)

        window,3
        coefs = jpm_polyfit(Xs[line_sort]-npts/2., $
                 line_list[associations[line_sort]], order, $
                 title=string("Dispersion relation for comp ", files[i]), $
                 xtitle='Pixels ref to center of image', $
                 ytitle='Best guess association to atlas line')
        print, coefs
        stop


        window,3
        plot, Xs[line_sort], line_list[associations[line_sort]], $
              yrange=[min(line_list[associations]), $
                      max(line_list[associations])], $
              psym=asterisk


        to_pass = { line_pix:Xs[line_sort], line_list:line_list, npts:npts }
        dispers = tnmin('line_correlate', dispers, $
                        FUNCTARGS=to_pass, /AUTODERIVATIVE)


        ;; Check again
        close_match = make_disp_axis(dispers, Xs, npts/2.)
        associations = list_associate(close_match, line_list)

        window,3
        plot, Xs[line_sort], line_list[associations[line_sort]], $
              yrange=[min(line_list[associations]), $
                      max(line_list[associations])], $
              psym=asterisk
        stop


;        plot,Xs[line_sort], line_list[atlas_idx], xstyle=2, ystyle=2, $
;             yrange=[min(line_list[atlas_idx]),max(line_list[atlas_idx])], $
;             psym=asterisk

;        window,7
;
;        xaxis = make_disp_axis(dispers, indgen(npts), npts/2.)
;        plot, xaxis, spec
;        oplot, xaxis, model_spec, linestyle=dotted
        
        
;        xaxis = make_disp_axis(dispers, Xs[line_sort], npts/2.)
;        plot, xaxis, spec






;        if min(line_strengths) eq max(line_strengths) then $
;          Yvals[*] = 1
;



;        ;; Haven't gotten this corelation code to work properly yet
;        for ds = 0,3 do begin
;           ideal_spec = smooth(deltafn(Xs, Yvals, Yaxis), (3-ds)*100)
;

;        openw, lun, /get_lun, temp[0]+'.comp.spec'
;        for ti=0, npts-1 do begin
;           printf, lun, spec[ti]
;        endfor
;       close, lun

;        
;        ;; Hone in on likely center wavelength by doing a correlation
;        ;; with the line list 
;        first_pass = fltarr(npts)
;        wspan = dispers[1] * npts
;        tdisp = dispers
;        tdisp[0] = dispers[0] - wspan/2.
;        for di = 0,npts-1 do begin
;           first_pass[di] = comp_correlate(tdisp, spec=spec, line_list=line_list);, line_stengths=line_stengths)
;           tdisp[0] = tdisp[0] + di*tdisp[1]
;        endfor
;        plot, first_pass
;        stop
;
;           to_pass = { spec:ideal_spec, line_list:line_list}
;           dispers = tnmin('comp_correlate', dispers, $
;                           FUNCTARGS=to_pass, /AUTODERIVATIVE, $
;                           /MAXIMIZE)
;           print,dispers
           disp_arrays[0:order,i] = dispers[*]
           ngood = ngood + 1
     endelse ;; CATCH if err
  endfor ;; all files in directory
  CATCH, /CANCEL
  dbclose

  if ngood eq 0 then message, 'ERROR: no properly prepared files found, database not updated'
  
  oldpriv=!priv
  !priv = 2
  dbopen, dbname, 1
  dbupdate, entries, 'cross_disp', disp_arrays
  dbclose
  !priv=oldpriv
  message, /INFORMATIONAL, 'Updated measured dispersion information rotation in ' + dbname
  
  ;; For convenience 
  message, /INFORMATIONAL, 'Directory is set to ' + indir

end

