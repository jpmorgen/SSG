;+
; NAME: ssg_blob_search
;
; PURPOSE: Search for blobs in the Io [OI] signal
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
; DESCRIPTION:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:  
;   Common blocks are ugly.  Consider using package-specific system
;   variables.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; ;; based on
; $Id: ssg_blip_search.pro,v 1.8 2015/03/04 15:50:32 jpmorgen Exp $
;
; $Log: ssg_blip_search.pro,v $
; Revision 1.8  2015/03/04 15:50:32  jpmorgen
; Summary: Last checkin before git
;
; Revision 1.7  2014/11/04 17:49:07  jpmorgen
; About to keep track of UT of blips to enable construction of a periodogram
;
; Revision 1.6  2013/04/29 16:39:47  jpmorgen
; About to add negative blips
;
; Revision 1.5  2013/01/31 13:39:20  jpmorgen
; About to add time limit
;
; Revision 1.4  2012/11/27 21:57:56  jpmorgen
; Fixed a bunch of stuff.  About to switch to portrait PS output
;
; Revision 1.3  2012/11/26 22:36:29  jpmorgen
; Version with bug fixed
;
; Revision 1.2  2012/11/26 22:36:10  jpmorgen
; Version I submitted for abstract
;
; Revision 1.1  2012/07/20 01:44:31  jpmorgen
; Initial revision
;
;-
pro ssg_smyth_chi2, $
   freed=freed, $ ;; Use Melanie's database
   nday1=nday, $
   ndays=ndays, $
   chi2s=chi2s, $
   long_3s=long_3s, $
   phis=phis, $
   sides=sides, $
   ps=ps, $
   jpeg=jpeg, $
   plot=plot, $
   phi_plot=phi_plot, $
   sys_III_bin=sys_III_bin, $
   sys_III_offset=sys_III_offset, $
   histogram=histogram, $
   _EXTRA=extra

  init = {tok_sysvar}
  init = {ssg_sysvar}


  ;; Make default nday behavior the whole range.  If just nday
  ;; is specified, only do that one day.
  case N_elements(nday) of
     0: begin
        nday_start = 0
        nday_end = 7000
     end
     1: begin
        nday_start = floor(nday)
        nday_end = nday_start
     end
     2: begin
        nday_start = floor(nday[0])
        nday_end = ceil(nday[1])
     end
  endcase  
  
  if N_elements(sys_III_bin) eq 0 then $
     sys_III_bin = 90

  if N_elements(sys_III_offset) eq 0 then $
     sys_III_offset = 0
  
  if N_elements(intensity_tolerance) eq 0 then $
     intensity_tolerance = 0.001

  ;; Might need to work with PS and X differently.  Color currently
  ;; works better with X.
  color
  ;;if !d.name eq 'X' then $
  ;;   device, true_color=24

  ;; Be polite with the color table
  tvlct, user_r, user_g, user_b, /get
  tek_color

  ;; Be polite, but get the line thicknesses we need for PS output
  ;; (leave for terminal output since that gets us started on PS output)
  oPthick     = !P.thick
  oPcharsize  = !P.charsize
  oPcharthick = !P.charthick
  oXthick     = !X.thick
  oYthick     = !Y.thick

  !P.thick = 3
  !P.charsize = 1.5
  !P.charthick = 2
  !X.thick = 2
  !Y.thick = 2      

  ;; Initialize output arrays for pfo_array_append
  ndays = 'None'
  chi2s = 'None'
  long_3s = 'None'
  phis = 'None'
  sides= 'None'

  ;; Read in the model results
  model_top = !ssg.top + path_sep() + 'analysis' + path_sep() + 'max' +  path_sep()
  ;; Max provides model results in consistent format, but I need to
  ;; make it into a 1-line format
  restore, model_top + 'britfil.dM2915.1line_template.sav'
  ;; m = Melanie, a = autofit
  mmodel = read_ascii(model_top + 'britfil.dM2915.1line', template=model_template)
  amodel = read_ascii(model_top + 'brit_set2_F1616.1line', template=model_template)
           
  dbclose ;; just in case
  ;; --> I will want to change this to the latest, once Max gets the
  ;; latest fitted
  adbname = '/data/io/ssg/analysis/archive/database/2015-09-19_to_max/io_oi_analyze'
  ;;adbname = 'io_oi_analyze'
  dbopen, adbname, 0
  oentries = dbfind("obj_code=1", dbfind("redchisq<5"))
  aentries = oentries
  ;; --> I want to ammend this when I use the April 2016
  ;; version because I don't filter based on nn_DOWL
  lentries = dbfind("nn_DOWL<-50", oentries)
  hentries = dbfind("nn_DOWL>50", oentries)
  aentries = [lentries, hentries]
  Nae = N_elements(aentries)
  bad_idx = where((aentries[1:Nae-1] - aentries[0:Nae-2]) lt 0, count)
  print, count
  ;; Extract the autofit entries once
  dbext, aentries, "nday, long_3, phi, intensity, err_intensity, alf, delta, wc, err_wc", andays, along_3s, aphis, aintensities, aerr_intensities, aalfs, adeltas, awcs, aerr_wcs
  ;;dbext, aentries, "nn_DOWL, nn_ew, nn_Dw, nn_Lw, redchisq", nn_DOWLs, nn_ews, nn_Dws, nn_Lws, redchisqs
  dbclose

  ;; Sort on nday here and apply that to all extracted quantities to
  ;; make sure everything is in the same order as Max's file
  sort_idx = sort(andays)
  andays                = andays[sort_idx]
  along_3s              = along_3s[sort_idx]        
  aphis                 = aphis[sort_idx]           
  aintensities          = aintensities[sort_idx]    
  aerr_intensities      = aerr_intensities[sort_idx]
  aalfs                 = aalfs[sort_idx]           
  adeltas               = adeltas[sort_idx]         
  awcs                  = awcs[sort_idx]            
  aerr_wcs              = aerr_wcs[sort_idx]        
  
  ;; Open Melanie's database and keep it open
  dbopen,'/data/io/ssg/analysis/mef/database/io6300_integrated'
  ;; Find all the Io [OI] measurements
  mentries = dbfind(/silent, "intensity>0.001", $             ;; screen out junk
                    dbfind(/silent, "lambda=6300", $          ;; make sure we have the right line
                           dbfind(/silent, "obj_code=1")))    ;; Io

  print, 'Total number of Freed Io [OI] points: ', N_elements(mentries)
  print, 'Total number of autofit Io [OI] points: ', N_elements(aentries)

  ;; Sat Apr  9 20:01:16 2016  jpmorgen@byted
  ;; See if I can fix Max's scale problems by preferring Melanie's,
  ;; where available.  Data points will be selected the other way
 
  ;; Handle each nday one at a time
  for inday=nday_start, nday_end do begin
     ;; Create the strings necessary to query the ZDBASE for nday
     ndayl = string(format='("nday>", i5)', inday)
     ndayh = string(format='("nday<", i5)', inday+1)
     OI = dbfind(/silent, ndayl, $ ;; correct nday
                 dbfind(/silent, ndayh, mentries), $
                 count=mdata_count)
     aidx = where(inday lt andays and andays le inday+1, adata_count)

     ;; Don't bother with ndays unless they have at least 3 points
     if mdata_count lt 3 and adata_count lt 3 then $
           CONTINUE

     ;; If we made it here, we have either Melanie or autofit data
     print, 'NDAY = ', inday

     ;; Extract quantities we care about.
     ;; Mon Apr  4 14:00:34 2016  jpmorgen@byted
     ;; --> Problem with Max's model scale factor when set to 2.3?
     if mdata_count gt adata_count + 10 or adata_count lt 5 then begin
     ;;if mdata_count gt adata_count then begin        ;; Melanie's has more 
     ;; Temporarily take out Melanie's results
     ;;if adata_count eq 0 then begin
        ;;print, 'No autofit data for this date'
        ;;CONTINUE
        print, 'Using Freed database'
        data_count = mdata_count
        dbext, OI, 'nday, long_3, intensity, err_intensity, fcont, err_fcont, wc, err_wc', mnday, mlong_3, mintensity, merr_intensity, mfcont, merr_fcont, mwc, merr_wc
        ;; ndays is causing trouble if not matched in type.  Make it
        ;; double so I don't dumb down autofit
        mnday = double(mnday)
        dbext, OI, 'nrows, numlines, deldot, deldot_m, err_deldot_m', mnrows, mnumlines, mdeldot, mdeldot_m, merr_deldot_m
        dbext, OI, 'phi, side', mphi, mside
        ;; Switch to her model
        model = mmodel
     endif else begin
        ;; Autofit has more.  Stuff things into m* variables, since
        ;; they are used below
        print, 'Using autofit database'
        data_count = adata_count
        mnday          = andays        [aidx]
        mlong_3        = along_3s       [aidx]
        mintensity     = aintensities    [aidx]
        merr_intensity = aerr_intensities[aidx]
        ;;mfcont         = afconts        [aidx]
        ;;merr_fcont     = aerr_fconts    [aidx]
        ;;mwc            = awcs           [aidx]
        ;;merr_wc        = aerr_wcs       [aidx]
        mphi           = aphis          [aidx]
        ;;mside          = asides         [aidx]
        ;; Switch to the autofit model
        model = amodel
     endelse

     

     ;; Find our overlap between the model and this nday

     ;; jdcnv provides answers in double precision, which have trouble
     ;; converting to integers the way I want, so tweak the hours a
     ;; little bit off of zero and then take the floor
     jdcnv, model.year, model.month, model.day, 0.1, model_jds
     model_indays = floor(model_jds - !ssg.JDnday0)

     model_nday_idx = where(model_indays eq inday, model_count)
     if model_count eq 0 then begin
        message, 'WARNING: no nday match found in model for nday ' + strtrim(inday, 2) + ' data_count = ' + strtrim(data_count, 2), /CONTINUE
        CONTINUE
     endif
     ;; Check to make sure we are really lined up
     if data_count ne model_count then begin
        ;; aligning to data barfed for the case of 2841, which had
        ;; missing data and missing model (not sure what that is all about).
        ;;if data_count lt model_count then begin
        ;;   message, 'WARNING: ' + strtrim(data_count, 2) + ' data points ' + strtrim(model_count, 2) + ' model points.  Aligning to data', /CONTINUE
        ;;   align_idx = 'None'
        ;;   for idata=0, data_count-1 do begin
        ;;      ;; Create an index into model_nday_idx for model points
        ;;      ;; that match the data
        ;;      this_align_idx = where(abs(model.data[model_nday_idx] - mintensity[idata]) $
        ;;                             le intensity_tolerance, count)
        ;;      if count eq 0 then begin
        ;;         message, 'WARNING: no model point found to match data point ' + strtrim(mintensity[idata], 2), /CONTINUE
        ;;         CONTINUE
        ;;      endif ;; no match
        ;;      pfo_array_append, align_idx, this_align_idx
        ;;   endfor ;; idata
        ;;   model_nday_idx = model_nday_idx[align_idx]
        ;;endif else begin
        ;; Just align to the model for now
        message, 'WARNING: data and model mismatch.  Aligning to model.', /CONTINUE
        align_idx = 'None'
        for imodel=0, model_count-1 do begin
           this_align_idx = where(abs(model.data[model_nday_idx[imodel]] - mintensity) $
                                  le intensity_tolerance, count)
           if count eq 0 then begin
              message, 'WARNING (for now): no data point found to match model point ' + strtrim(model.data[imodel], 2), /CONTINUE
              CONTINUE
           endif
           pfo_array_append, align_idx, this_align_idx
        endfor ;; imodel
        mnday          = mnday         [align_idx]
        mlong_3        = mlong_3       [align_idx]
        mintensity     = mintensity    [align_idx]
        merr_intensity = merr_intensity[align_idx]
        ;;mfcont         = mfcont        [align_idx]
        ;;merr_fcont     = merr_fcont    [align_idx]
        ;;mwc            = mwc           [align_idx]
        ;;merr_wc        = merr_wc       [align_idx]
        mphi           = mphi          [align_idx]
        ;;mside          = mside         [align_idx]
        ;;endelse ;; aligning to model
     endif ;; aligning to data vs. model
     ;; Make a new variable, scaled_model, which is the raw model
     ;; multiplied by the scale factor which we will compare to the
     ;; data.  All indices into scaled_model will match those into
     ;; the database quantities we extract below
     scaled_model = model.model_scale1[model_nday_idx] * model.model_scale[model_nday_idx]
     ;; Construct the chi^2 for each entry in this day
     chi2 = ((mintensity - scaled_model) / merr_intensity)^2.

     ;; Accumulate results
     pfo_array_append, ndays, mnday
     pfo_array_append, chi2s, chi2
     pfo_array_append, long_3s, mlong_3
     pfo_array_append, phis, mphi
     ;;pfo_array_append, sides, mside

     
     ;; Plot individual day, if desired
     if keyword_set(plot) then begin
        ;; Initialize postscipt output
        if size(plot, /TNAME) eq 'STRING' then begin
           set_plot, 'ps'
           device, /portrait, filename=plot, /color, /encap
        endif

        ic = 1
        format = '(i4, "/", i2, "/", i2, " Scale ", f3.1, "x10^15 !7u!6", i4, " -", i4)'
        title = string(format=format, $
                       model.year[model_nday_idx[0]], $
                       model.month[model_nday_idx[0]], $
                       model.day[model_nday_idx[0]], $
                       model.model_scale[model_nday_idx[0]], $
                       phis[0], phis[N_elements(phis)-1])
        plot, [0], [0], psym=!tok.dot, $
              xtickinterval=90, xrange=[0,360], yrange=[0,25], $
              xtitle='!7k!6!DIII!N', ytitle='Io [OI] 6300 !3' + !tok.angstrom + ' !6(kR)', $
              title=title, position = [0.11, 0.15, 0.95, 0.9]
        
        ;; Temperarily calculated chi2 so I can plot that to see why I
        ;; am not seeing some of the days I used to see on the chi2 plot
        ;;chi2 = ((mintensity - scaled_model) / merr_intensity)^2.
        ;; --> technically, this should be mphis, but I just do things
        ;; one day at a time for now, so this will work...
        plot_idx = where(phis gt 360 - sys_III_offset or $
                         (0 lt phis and phis lt ic*sys_III_bin - sys_III_offset), count)
        if count gt 0 then begin
           pfo_array_append, legend_colors, ic
           pfo_array_append, legend_text, $
                             string(format='(i4, " < !7u!6 < ", i4)', $
                                    360 - sys_III_offset, $
                                    ic*sys_III_bin - sys_III_offset)
           oplot, mlong_3[plot_idx], mintensity[plot_idx], psym=!tok.plus
           errplot, mlong_3[plot_idx], mintensity[plot_idx]-merr_intensity[plot_idx]/2., mintensity[plot_idx]+merr_intensity[plot_idx]/2., linestyle=!tok.solid
           oplot, mlong_3[plot_idx], scaled_model[plot_idx], psym=!tok.triangle
           oplot, mlong_3[plot_idx], chi2s[plot_idx]/10., psym=!tok.square, color=!tok.orange
        endif
        for ic=2,360/sys_III_bin do begin
           plot_idx = where((ic-1)*sys_III_bin - sys_III_offset lt phis and $
                            phis lt ic*sys_III_bin - sys_III_offset, count)
           if count gt 0 then begin
              pfo_array_append, legend_colors, ic
              pfo_array_append, legend_text, $
                                string(format='(i4, " < !7u!6 < ", i4)', $
                                       (ic-1)*sys_III_bin - sys_III_offset, $
                                       ic*sys_III_bin - sys_III_offset)
              oplot, mlong_3[plot_idx], mintensity[plot_idx], psym=!tok.plus, color=ic
              errplot, mlong_3[plot_idx], mintensity[plot_idx]-merr_intensity[plot_idx]/2., mintensity[plot_idx]+merr_intensity[plot_idx]/2., linestyle=!tok.solid, color=ic
              oplot, mlong_3[plot_idx], scaled_model[plot_idx], psym=!tok.triangle, color=ic
              oplot, mlong_3[plot_idx], chi2s[plot_idx]/10., psym=!tok.square, color=!tok.orange
           endif
        endfor ;; each phi bin

        al_legend, legend_text, $
                   psym=replicate(!tok.triangle, N_elements(legend_colors)), colors=byte(legend_colors)

        if size(plot, /TNAME) eq 'STRING' then begin
           device,/close
           set_plot, 'x' 
        endif  

        !P.thick     = oPthick    
        !P.charsize  = oPcharsize 
        !P.charthick = oPcharthick
        !X.thick     = oXthick    
        !Y.thick     = oYthick    

        ;; Return color table to its original value
        tvlct, user_r, user_g, user_b

     endif ;; plot individual days

  endfor  ;; each nday

  dbclose

  ;; If we were using the plotting feature for individual ndays,
  ;; return now
  if keyword_set(plot) then $
     return
  
  ;; Plot chi^s vs. phi
  if keyword_set(phi_plot) then begin
     ;; plot histograms of chi^s vs phi
     if keyword_set(histogram) then begin
        ic = 1
        plot_idx = where(phis gt 360 - sys_III_offset or $
                         (0 lt phis and phis lt ic*sys_III_bin - sys_III_offset))
        h = histogram(chi2s[plot_idx], binsize=1, locations=locations)
        h /= float(N_elements(plot_idx))
        plot, locations, h, psym=!tok.square, xtitle='chi-square', ytitle='!6Normalized histogram', position = [0.15, 0.15, 0.95, 0.95], xrange=[0,40]
        for ic=2,360/sys_III_bin do begin
           plot_idx = where((ic-1)*sys_III_bin - sys_III_offset lt phis and $
                            phis lt ic*sys_III_bin - sys_III_offset)
           h = histogram(chi2s[plot_idx], binsize=1, locations=locations)
           h /= float(N_elements(plot_idx))
           oplot, locations, h, psym=!tok.square, color=ic
        endfor ;; each phi bin
        return           
     endif ;; Plot chi^s vs. phi

     ;; If we made it here, we want to just plot all the chi2s as a
     ;; function of phis together
     plot, phis, chi2s, psym=!tok.square, xtitle='Io orbital phase', ytitle='!7v!6!U2!N!6chi-square', yrange=[1, 150], xstyle=!tok.exact, ystyle=!tok.exact, xtickinterval=90, position = [0.15, 0.15, 0.95, 0.95]
     
     return
  endif

  ;; If we made it here we want to plot chisq vs sysIII

  ;; Initialize postscipt output
  if keyword_set(ps) then begin
     if size(ps, /TNAME) ne 'STRING' then $
        ps = 'ssg_smyth_chi2_out.ps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /color, /encap
  endif

  ;; Divide phi bins up into sys_III_bin deg chunks.  If
  ;; sys_III_offset is set, shift by that amount
  ic = 1
  plot_idx = where(phis gt 360 - sys_III_offset or $
                   (0 lt phis and phis lt ic*sys_III_bin - sys_III_offset))
  pfo_array_append, legend_colors, ic
  pfo_array_append, legend_text, $
                    string(format='(i4, " < !4u!6 < ", i4)', $
                           360 - sys_III_offset, $
                           ic*sys_III_bin - sys_III_offset)

  ;;wset,ic
  ;;plot, long_3s[plot_idx], chi2s[plot_idx], psym=!tok.square, xtitle='System III', ytitle=string('chi-square'), yrange=[1, 300], xstyle=!tok.exact, ystyle=!tok.exact, /ylog
  plot, long_3s[plot_idx], chi2s[plot_idx], $
        psym=!tok.square, xtitle='!7k!6!DIII!N', ytitle='!7v!6!U2!N!6', $
        xrange=[0,360], yrange=[1, 150], xstyle=!tok.exact, ystyle=!tok.exact, $
        xtickinterval=90, position = [0.15, 0.15, 0.95, 0.95]
  ;;plot, [0,360],[0,0], psym=!tok.dot, xtitle='System III', ytitle='!6chi-square', yrange=[1, 300], xstyle=!tok.exact, ystyle=!tok.exact, xtickinterval=90, position = [0.15, 0.15, 0.9, 0.9]
  ;;oplot, long_3s[plot_idx], chi2s[plot_idx], psym=!tok.square, color=8
  for ic=2,360/sys_III_bin do begin
     plot_idx = where((ic-1)*sys_III_bin - sys_III_offset lt phis and $
                      phis lt ic*sys_III_bin - sys_III_offset, count)
     if count eq 0 then $
        CONTINUE
     oplot, long_3s[plot_idx], chi2s[plot_idx], psym=!tok.square, color=ic
     ;; if !d.name eq 'X' then $
     ;;    wset, ic
     ;; plot, long_3s[plot_idx], chi2s[plot_idx], psym=!tok.square, xtitle='System III', ytitle=string('chi-square, phi = ', 45*(ic-1), 45*ic), yrange=minmax(chi2s), color=ic, xtickinterval=90, position = [0.15, 0.15, 0.9, 0.9]
     pfo_array_append, legend_colors, ic
     pfo_array_append, legend_text, $
                       string(format='(i4, " < !4u!6 < ", i4)', $
                              (ic-1)*sys_III_bin - sys_III_offset, $
                              ic*sys_III_bin - sys_III_offset)
  endfor

  al_legend, legend_text, position=[100, 140], /top, $
             psym=replicate(!tok.triangle, N_elements(legend_colors)), colors=byte(legend_colors)

  if keyword_set(ps) then begin
     device,/close
     set_plot, 'x' 
  endif  

  !P.thick     = oPthick    
  !P.charsize  = oPcharsize 
  !P.charthick = oPcharthick
  !X.thick     = oXthick    
  !Y.thick     = oYthick    

  ;; Return color table to its original value
  tvlct, user_r, user_g, user_b

end
