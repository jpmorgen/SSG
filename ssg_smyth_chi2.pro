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

;; Helper function to get white and black sorted out on screen vs. ps output
function black_white, colors
  init = {tok_sysvar}

  ;; I tend to debug on screen, so things look fine here
  if !d.name eq 'X' then $
     return, colors
  ;; If we made it here, we need to flip black and white for PS
  ;; output, since paper is white....
  black_idx = where(colors eq !tok.black, black_count)
  white_idx = where(colors eq !tok.white, white_count)
  if black_count gt 0 then begin
     print, 'Am I here?'
     colors[black_idx] = !tok.white
  endif
  if white_count gt 0 then $
     colors[white_idx] = !tok.black
  return, colors
end


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
   show_chi2=show_chi2, $ ;; show chi^2 in orange boxes on individual day plots
   phi_plot=phi_plot, $
   sys_III_bin=sys_III_bin, $
   sys_III_offset=sys_III_offset, $
   histogram=histogram, $
   min_data_count=min_data_count, $ ;; minimum number of datapoints per night
   nday_threshold=nday_threshold, $ ;; nday gap threshold in units of median nday difference for exposure calculations
   min_cadence=min_cadence, $ ;; for exposure calculations 
   scale_vs_time=scale_vs_time, $ ;; plot model scale factor vs time
   inner_torus=inner_torus, $ ;; These are defined by phi
   outer_torus=outer_torus, $
   straddle=straddle, $ ;; try to find days that straddle outer to inner transition
   rinner_torus=rinner_torus, $ ;; These are defined by radius
   router_torus=router_torus, $
   rstraddle=rstraddle, $ ;; try to find days that straddle outer to inner transition
   whole_nights=whole_nights, $ ;; work with whole nights after applying rstraddle criterion
   model_scales=model_scales, $
   plasma_rs=plasma_rs, $ ;; plasmacentric radial coordinate from model
   scale_inner=scale_inner, $
   new_scale=new_scale, $ ;; for applying a new scale factor to individual plots
   nn_DOWL=nn_DOWL, $
   legend=legend, $ ;; general legend for [OI] intensity plots
   max_model=max_model, $
  _EXTRA=extra

  init = {tok_sysvar}
  init = {ssg_sysvar}

  ;; plasma-centric radius of nominal peak electron density (Rj)
  e_peak_r = 5.72

  ;; Should do this for the phi-defined ones too, though I hope those
  ;; become obsolete
  if keyword_set(rinner_torus) + keyword_set(router_torus) + keyword_set(rstraddle) gt 1 then $
     message, 'ERROR: specify only one of rinner_torus, router_torus, rstraddle'
  
  ;; Handle /straddle case with a default
  if N_elements(straddle) eq 1 then $
     straddle = [15, 45]

  if N_elements(nday_threshold) eq 0 then $
     nday_threshold = 2

  ;; Minimum average cadence in minutes for decent sampling.  --> Can
  ;; possibly go with longer
  if N_elements(min_cadence) eq 0 then $
     min_cadence = 40

  ;; Handle the case where a string nday is input
  if size(/type, nday) eq !tok.string then begin
  endif


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

  if N_elements(long_3_tolerance) eq 0 then $
     long_3_tolerance = 0.01

  if N_elements(max_model) eq 0 then $
     max_model = 'brit_set3_D116'

  ;; Might need to work with PS and X differently.  Color currently
  ;; works better with X.
  color
  ;;if !d.name eq 'X' then $
  ;;   device, true_color=24

  ;; Be polite with the color table
  tvlct, user_r, user_g, user_b, /get
  tek_color

  ;; Initialize postscript output
  if keyword_set(ps) or size(plot, /type) eq !tok.string then begin
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

     if size(ps, /type) ne !tok.string then $
        ps = 'ssg_smyth_chi2_out.ps'
     set_plot, 'ps'
     device, /portrait, filename=ps, /color, /encap
  endif

  ;; Initialize output arrays for pfo_array_append
  ndays = 'None'
  chi2s = 'None'
  long_3s = 'None'
  phis = 'None'
  sides= 'None'

  ;; Read in the model results
  model_top = !ssg.top + path_sep() + 'analysis' + path_sep() + 'max' +  path_sep()
  ;; Max provided model results in consistent format, but I needed to
  ;; make it into a 1-line format
  restore, model_top + 'britfil.dM2915.1line_template.sav'
  ;; m = Melanie, a = autofit
  mmodel = read_ascii(model_top + 'britfil.dM2915.1line', template=model_template)
  ;;amodel = read_ascii(model_top + 'brit_set2_F1616.1line', template=model_template)
  ;; Starting with the spring 2016 run, Max generates his own 1-line
  ;; version
  ;;restore, model_top + 'max_1_line_template.sav'
  ;;amodel = read_ascii(model_top + 'brit_set3_N216', template=max_1_line_template)           
  restore, model_top + 'max_1_line_template_r.sav'
  amodel = read_ascii(model_top + max_model, template=max_1_line_template_r)           
  dbclose ;; just in case
  ;; --> I will want to change this to the latest, once Max gets the
  ;; latest fitted
  ;;adbname = '/data/io/ssg/analysis/archive/database/2015-09-19_to_max/io_oi_analyze'
  ;;adbname = '/data/io/ssg/analysis/archive/database/2016-04-20/io_oi_analyze'
  ;;adbname = '/data/io/ssg/analysis/archive/database/2016-04-20_nday_3571_GW_75/io_oi_analyze'
  adbname = 'io_oi_analyze'
  dbopen, adbname, 0
  oentries = dbfind("err_intensity<10", dbfind("redchisq<10", dbfind("obj_code=1")))
  ;;oentries = dbfind("err_intensity<10", dbfind("redchisq<4", dbfind("obj_code=1")))
  if keyword_set(inner_torus) then $
     oentries = dbfind("io_phi<180", dbfind("io_phi>30", oentries))
  if keyword_set(outer_torus) then $
     oentries = [dbfind("io_phi>180", oentries), dbfind("io_phi<30", oentries)]
  if keyword_set(straddle) then $
     oentries = dbfind("io_phi<" + strtrim(straddle[1], 2), dbfind("io_phi>" + strtrim(straddle[0], 2), oentries))
  aentries = oentries
  ;; Allow nn_DOWL to be a keyword
  if keyword_set(nn_DOWL) then begin
     lentries = dbfind("nn_DOWL<" + strtrim(-1. * nn_DOWL, 2), oentries)
     hentries = dbfind("nn_DOWL>" + strtrim(nn_DOWL, 2), oentries)
     aentries = [lentries, hentries]
  endif ;; nn_DOWL
  Nae = N_elements(aentries)
  ;;bad_idx = where((aentries[1:Nae-1] - aentries[0:Nae-2]) lt 0, count)
  ;;print, count
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

  if keyword_set(inner_torus) then $
     mentries = dbfind("phi<180", dbfind("phi>30", mentries))
  if keyword_set(inner_torus) then $
     mentries = [dbfind("phi>180", mentries), dbfind("phi<30", mentries)]
  if keyword_set(straddle) then $
     mentries = dbfind("phi<" + strtrim(straddle[1], 2), dbfind("phi>" + strtrim(straddle[0], 2), mentries))

  print, 'Total number of Freed Io [OI] points: ', N_elements(mentries)
  print, 'Total number of autofit Io [OI] points: ', N_elements(aentries)

  ;; Sat Apr  9 20:01:16 2016  jpmorgen@byted
  ;; See if I can fix Max's scale problems by preferring Melanie's,
  ;; where available.  Data points will be selected the other way
 
  ;; Keep track of number of nights and exposure time
  nday_counter = 0
  exp_time = 0
  N_total_obs = 0
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
     if NOT keyword_set(min_data_count) then $
        min_data_count = 3
     if mdata_count lt min_data_count and adata_count lt min_data_count then $
        CONTINUE

     ;; If we made it here, we have either Melanie or autofit data
     print, 'NDAY = ', inday

     ;; Extract quantities we care about.

     ;; Generally avoid Melanie's database, since it has
     ;; problems with phase and doesn't have some of the fancy
     ;; quantities like plasmacentric radius of Io for some of our
     ;; other options.  But it does give us the option to have more data
     if keyword_set(freed) then begin
        use_freed = (mdata_count gt adata_count + 10 or adata_count lt 5) and mdata_count gt adata_count
     endif else begin
        if adata_count eq 0 then begin
           print, 'No autofit data for ' + nday2date(inday)
           CONTINUE
        endif
        ;; If we made it here, we have autofit data, but don't
        ;; want to use Melanie Freed's database
        use_freed = 0
     endelse
     if use_freed then begin
        print, 'Using Freed database'
        database = 'Freed'
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
        database = 'Auto'
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
     endelse ;; Freed vs. autofit

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
     if model_count lt min_data_count then begin
        message, /INFORMATIONAL, 'NOTE: number of model points below min_data_count of ' + strtrim(min_data_count, 2) + ' skipping this day'
        CONTINUE
     endif
     ;; Check to make sure we are really lined up
     if data_count ne model_count then begin
        ;; aligning to data barfed for the case of 2841, which had
        ;; missing data and missing model (not sure what that is all about).
        if data_count lt model_count then begin
           message, 'WARNING: ' + strtrim(data_count, 2) + ' data points ' + strtrim(model_count, 2) + ' model points.  Aligning to data', /CONTINUE
           align_idx = 'None'
           for idata=0, data_count-1 do begin
              ;; Create an index into model_nday_idx for model points
              ;; that match the data.  Have to do this differently for
              ;; Freed and Auto, since Max didn't supply sysIII
              ;; coordinates for Freed and Auto precision on intensity
              ;; is too low
              if database eq 'Freed' then begin
                 this_align_idx = where(abs(model.data[model_nday_idx] - mintensity[idata]) $
                                        le intensity_tolerance, count)
              endif else begin
                 this_align_idx = where(abs(model.sysIII[model_nday_idx] - mlong_3[idata]) $
                                        le long_3_tolerance, count)
              endelse
              if count eq 0 then begin
                 message, 'WARNING: no model point found to match data point ' + strtrim(mintensity[idata], 2) + '.  Removing data point', /CONTINUE
                 mintensity[idata] = !values.f_NAN
                 CONTINUE
              endif ;; no match
              if count gt 1 then $
                 message, 'ERROR: duplicate entries found.  intensity_tolerance or long_3_tolerance too large'
              pfo_array_append, align_idx, this_align_idx
           endfor ;; idata
           ;; Erase data points not fitted by model
           good_data_idx = where(finite(mintensity) eq 1, count)
           if count eq 0 then begin
              message, 'ERROR: no good points left on this nday.  Something weird is happening', /CONTINUE
              CONTINUE
           endif
           mintensity = mintensity[good_data_idx]
           ;; Collect aligned model_idx
           model_nday_idx = model_nday_idx[align_idx]
        endif else begin
           message, 'WARNING: data and model mismatch.  Aligning to model.', /CONTINUE
           align_idx = 'None'
           for imodel=0, model_count-1 do begin
              if database eq 'Freed' then begin
                 this_align_idx = where(abs(model.data[model_nday_idx[imodel]] - mintensity) $
                                        le intensity_tolerance, count)
              endif else begin
                 this_align_idx = where(abs(model.sysIII[model_nday_idx[imodel]] - mlong_3) $
                                        le long_3_tolerance, count)
              endelse
              if count eq 0 then begin
                 message, 'WARNING: no data point found to match model point ' + strtrim(model.data[model_nday_idx[imodel]], 2) + '.  Removing model point', /CONTINUE
                 model_nday_idx[imodel] = -1
                 CONTINUE
              endif
              if count gt 1 then $
                 message, 'ERROR: duplicate entries found.  intensity_tolerance or long_3_tolerance too large'
              pfo_array_append, align_idx, this_align_idx
           endfor ;; imodel
           ;; Erase any model points that have no data
           good_model_idx = where(model_nday_idx ne -1, count)
           if count eq 0 then begin
              message, 'ERROR: no good points left on this nday.  Something weird is happening.  Skipping nday', /CONTINUE
              CONTINUE
           endif
           model_nday_idx = model_nday_idx[good_model_idx]
           ;; Collect aligned data
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
        endelse ;; aligning to model
     endif ;; aligning to data vs. model

     ;; Tweak the scale factor, if desired
     if keyword_set(plot) and keyword_set(new_scale) then begin
        model.model_scale[model_nday_idx] = new_scale
     endif
     
     ;; Cleaner to code radius-based inner/outer/straddle section this way
     if keyword_set(rinner_torus) + keyword_set(router_torus) + keyword_set(rstraddle) eq 1 then begin
        if keyword_set(rstraddle) then $
           if rstraddle eq 1 then $
              rstraddle = 0.05
        ;; Work with the inner/outer torus as defined by the model
        if keyword_set(rinner_torus) then $
           subset_idx = where(model.plasma_r[model_nday_idx] le e_peak_r, count)
        if keyword_set(router_torus) then $
           subset_idx = where(model.plasma_r[model_nday_idx] gt e_peak_r, count)
        if keyword_set(rstraddle) then $
           subset_idx = where(e_peak_r - rstraddle lt model.plasma_r[model_nday_idx] and $
                              model.plasma_r[model_nday_idx] lt e_peak_r + rstraddle, count)
        if count eq 0 then begin
           message, /INFORMATIONAL, 'NOTE: after applying inner/outer/straddle conditions, no points found.  Skipping this day.'
           CONTINUE
        endif
        ;; Check to see if we want to get back to working with the entire nday
        if keyword_set(rstraddle) + keyword_set(whole_nights) eq 2 then $
           subset_idx = indgen(N_elements(model_nday_idx))

        ;; If we made it here, we have something to do
        ;; Take out subset of data and model noting that subset_idx is
        ;; into model_inday_idx and just plane data
        model_nday_idx = model_nday_idx[subset_idx]
        ;; Collect aligned data
        mnday          = mnday         [subset_idx]
        mlong_3        = mlong_3       [subset_idx]
        mintensity     = mintensity    [subset_idx]
        merr_intensity = merr_intensity[subset_idx]
        ;;mfcont         = mfcont        [subset_idx]
        ;;merr_fcont     = merr_fcont    [subset_idx]
        ;;mwc            = mwc           [subset_idx]
        ;;merr_wc        = merr_wc       [subset_idx]
        mphi           = mphi          [subset_idx]
        ;;mside          = mside         [subset_idx]
        plasma_r	= model.plasma_r[model_nday_idx]
     endif ;; radius-based inner/outer/straddle

     ;; After we have pruned everything, check if we have anything left
     N_nday = N_elements(mnday)
     if N_nday lt min_data_count then begin
        message, /INFORMATIONAL, 'NOTE: after cleaning, number of model points below min_data_count of ' + strtrim(min_data_count, 2) + ' skipping this day'
        CONTINUE
     endif

     ;; At this point, all of the model scales are the same for the
     ;; day.  We are about to mess with this, so save off the original
     ;; model scale for title purposes
     model_scale = model.model_scale[model_nday_idx[0]]

     ;; Now that we are aligned, scale the model for inner torus, if desired
     if keyword_set(scale_inner) then begin
        ;;inner_idx = where(30 lt mphi and mphi lt 180, count)
        inner_idx = where(model.plasma_r[model_nday_idx] lt e_peak_r, count)
        if count gt 0 then $
           model.model_scale[model_nday_idx[inner_idx]] *= scale_inner
     endif

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
     ;; Model scale factors for scale_vs_time plot
     pfo_array_append, model_scales, model.model_scale[model_nday_idx]
     pfo_array_append, plasma_rs, plasma_r

     ;; Exposure calculations
     nday_counter += 1
     ;; Find time differences between adjacent points to calculate
     ;; the amount of decent exposure time coverage (code from
     ;; ssg_blob_search)

     nday_diff = mnday[1:N_nday-1] - mnday[0:N_nday-2]
     ;; Generally we have a constant cadance with occational large
     ;; pauses.  Use median instead of the mean to spot the right
     ;; side of our gaps.  This is the index into nday_diff, but it is
     ;; labeled as the gap_right_idx, since it is to be used as the
     ;; index into parent arrays
     gap_right_idx = where(nday_diff gt nday_threshold*median(nday_diff), ntime_segments)

     if ntime_segments eq 0 then begin
        ;; If no gaps, replace where's -1 with the right bound of the
        ;; array
        gap_right_idx = N_nday-1
     endif else begin
        ;; If gaps, append the right bound of the array to our gap
        ;; list since where won't find that
        pfo_array_append, gap_right_idx, N_nday-1
     endelse
     ntime_segments += 1
     
     ;; Put the left side of our first interval at idx=0
     gap_left_idx = 0
     for igap=0,ntime_segments-1 do begin
        ;; Regenerate idx for this particular time segment
        N_in_gap = gap_right_idx[igap] - gap_left_idx + 1
     
        ;; Don't bother for gaps that have less than 3 points
        if N_in_gap lt 3 then begin
           message, /INFORMATIONAL, 'NOTE: skipping gap with only ' + strtrim(N_in_gap, 2) + ' points'
           ;; Move the left side of our gap forward if we have any more
           ;; gaps to process
           if igap lt ntime_segments then $
              gap_left_idx = gap_right_idx[igap] + 1
           CONTINUE
        endif
     
        idx = indgen(N_in_gap) + gap_left_idx
        N_total_obs += N_in_gap
        ;;print, 'new segment: ', idx
        ;; Check to see if the sampling in this gap is close enough to
        ;; our desired minimum cadence (in minutes)
        cadence = mean(nday_diff[gap_left_idx:gap_right_idx[igap]-1]) * 24.*60.
        if cadence gt min_cadence then begin
           message, /INFORMATIONAL, 'NOTE: skipping gap which has cadence of ' + strtrim(cadence, 2) + ' minutes'
           CONTINUE
        endif
        ;; If we made it here, we have a viable time segment
        exp_time += mnday[gap_right_idx[igap]] - mnday[gap_left_idx]
        ;; Move the left side of our gap forward if we have any more
        ;; gaps to process
        if igap lt ntime_segments then $
           gap_left_idx = gap_right_idx[igap] + 1
     endfor ;; each time segment

     ;; Plot individual day, if desired
     if keyword_set(plot) then begin
        ;; Initialize postscipt output
        if size(plot, /type) eq !tok.string then begin
           set_plot, 'ps'
           device, /portrait, filename=plot, /color, /encap
        endif

        ic = 1
        format = '(i4, "/", i2, "/", i2, " Scale ", f3.1, "x10^15 !7u!6", i4, " -", i4)'
        ;; Set the title model_scale to the new_scale value
        if keyword_set(new_scale) then $
           model_scale = new_scale

        title = string(format=format, $
                       model.year[model_nday_idx[0]], $
                       model.month[model_nday_idx[0]], $
                       model.day[model_nday_idx[0]], $
                       model_scale, $
                       phis[0], phis[N_elements(phis)-1])
        xtitle = '!7k!6!DIII!N'
        if keyword_set(scale_inner) then $
           xtitle = string(format='(a, "; Inner torus scaled by ", f3.1)', xtitle, scale_inner)
        plot, [0], [0], psym=!tok.dot, $
              xtickinterval=90, xrange=[0,360], yrange=[0,25], $
              xtitle=xtitle, ytitle='Io [OI] 6300 !3' + !tok.angstrom + ' !6(kR)', $
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
           ;; Plot inner or outer torus model points differently 
           inner_idx = where(model.plasma_r[model_nday_idx[plot_idx]] lt e_peak_r, n_inner, complement=outer_idx, ncomplement=n_outer)
           if n_inner gt 0 then $
              oplot, mlong_3[plot_idx[inner_idx]], scaled_model[plot_idx[inner_idx]], psym=!tok.asterisk
           if n_outer gt 0 then $
              oplot, mlong_3[plot_idx[outer_idx]], scaled_model[plot_idx[outer_idx]], psym=!tok.triangle
           if keyword_set(show_chi2) then $
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
              ;; Plot inner or outer torus model points differently 
              inner_idx = where(model.plasma_r[model_nday_idx[plot_idx]] lt e_peak_r, n_inner, complement=outer_idx, ncomplement=n_outer)
              if n_inner gt 0 then $
                 oplot, mlong_3[plot_idx[inner_idx]], scaled_model[plot_idx[inner_idx]], psym=!tok.asterisk, color=ic
              if n_outer gt 0 then $
                 oplot, mlong_3[plot_idx[outer_idx]], scaled_model[plot_idx[outer_idx]], psym=!tok.triangle, color=ic
              if keyword_set(show_chi2) then $
                 oplot, mlong_3[plot_idx], chi2s[plot_idx]/10., psym=!tok.square, color=!tok.orange
           endif
        endfor ;; each phi bin

        ;; Model point legend suggested by Marissa.  Hide the dot as
        ;; black (white in PS) so the Smyth & Marconi model line looks
        ;; like a title.
        al_legend, ['Data', 'Smyth & Marconi model:', 'Outer torus', 'Inner torus'], $
                   psym=[!tok.plus, !tok.dot, !tok.triangle, !tok.asterisk], $
                   colors=black_white([!tok.white, !tok.black, !tok.white, !tok.white]), $
                   charsize=1.3, /left
           
        ;; Phi & data legend
        al_legend, legend_text, $
                   psym=[replicate(!tok.triangle, N_elements(legend_colors))], $
                   colors=black_white(byte(legend_colors)), $
                   /right

        if size(plot, /type) eq !tok.string then begin
           device,/close
           set_plot, 'x'
        endif  

        ;; Return color table to its original value
        tvlct, user_r, user_g, user_b

        ;; Print out plot points with ndays and times
        print, 'nday, sysIII, phi, scale factor'
        for ipt=0, N_elements(ndays)-1 do begin
           print, format='(f10.4, 3f7.1)', $
                  ndays[ipt], long_3s[ipt], phis[ipt], model_scales[ipt] ;;, plasma_rs
        endfor
     endif ;; plot individual days

  endfor  ;; each nday
  message, /CONTINUE, 'NOTE: Found ' + strtrim(nday_counter, 2) + ' nights with min_data_count gt ' + strtrim(min_data_count, 2) + '.  Total exposure time approx ' + strtrim(exp_time * 24. , 2)+ ' hours.  Total number of points = ' + strtrim(N_total_obs, 2)

  dbclose

  ;; --> It would be nice if I had better logic to select the various
  ;; plotting options

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
                         (0 lt phis and phis lt ic*sys_III_bin - sys_III_offset), count)
        if count gt 0 then begin
           h = histogram(chi2s[plot_idx], binsize=1, locations=locations)
           h /= float(N_elements(plot_idx))
        endif else begin
           locations = [0]
           h = locations
        endelse
        plot, locations, h, psym=!tok.square, xtitle='chi-square', $
              ytitle='!6Normalized histogram', $
              position = [0.15, 0.15, 0.95, 0.95], $
              xrange=[0.000001,200], /xlog,$
              yrange=[0.001, 1], /ylog
        for ic=2,360/sys_III_bin do begin
           plot_idx = where((ic-1)*sys_III_bin - sys_III_offset lt phis and $
                            phis lt ic*sys_III_bin - sys_III_offset, count)
           if count eq 0 then $
              CONTINUE
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

  if keyword_set(scale_vs_time) then begin
     plot, !ssg.JDnday0 + ndays, model_scales, $
           xrange=[nday_start, nday_end] + !ssg.JDnday0, $
           yrange=yrange, $
           xtickunits = ['hours', 'Days', 'Months', 'Years'], $
           xtitle='UT date (Year, Month, Day, Hour)', $
           ytitle='Model scale factor', $
           ymargin=[15,2], $
           psym=!tok.plus, $ 
           xstyle=!tok.exact+!tok.extend, $
           ystyle=!tok.extend, $
           _EXTRA=extra
     return
  endif ;; scale_vs_time plot

  ;; If we made it here we want to plot chisq vs sysIII

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
  plot, long_3s[plot_idx], chi2s[plot_idx], /ylog, $
        psym=!tok.square, xtitle='!7k!6!DIII!N', ytitle='!7v!6!U2!N!6', $
        xrange=[0,360], yrange=[0.1, 200], xstyle=!tok.exact, ystyle=!tok.exact, $
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

  al_legend, legend_text, /right, $ ;; position=[100, 140], /top, $
             psym=replicate(!tok.square, N_elements(legend_colors)), $
             colors=black_white(byte(legend_colors))

  if keyword_set(ps) or size(plot, /type) eq !tok.string then begin
     device,/close
     set_plot, 'x'

     !P.thick     = oPthick    
     !P.charsize  = oPcharsize 
     !P.charthick = oPcharthick
     !X.thick     = oXthick    
     !Y.thick     = oYthick    
  endif  

  ;; Return color table to its original value
  tvlct, user_r, user_g, user_b

end
