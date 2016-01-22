;;
;;+
;; $Id: ssg_fit.pro,v 1.3 2015/03/04 15:50:06 jpmorgen Exp $
;
;; ssg_fit.  Use MPFITFUN to fit the Io spectra
;; nday_range can just be the starting nday
;
;;-

pro ssg_fit, nday_start_or_range, non_interactive=non_interactive, $
             no_plot=no_plot, _EXTRA=extra

                                ;ON_ERROR, 2
  if NOT keyword_set(nday_start_or_range) then $
     nday_start_or_range = [0,36500] ; Somewhat generous :-)

  nday_range = nday_start_or_range
  if N_elements(nday_range) eq 1 then $
     nday_range = [nday_start_or_range, 36500]

  first_time = 1
  repeat begin
     ;; Select one or more points
     ndays = ssg_select(nday_range, non_interactive=non_interactive, full_fnames=full_fnames)
     nndays = N_elements(ndays)
     ;; Fit each one, one by one, passing on command-line parameters
     ;; throught the _EXTRA mechanism
     for inday=0, nndays-1 do begin
        ;; Make a window the first time through unless you
        ;; don't want a window.  Code in ssg_fit1spec wsets to
        ;; this window if no_plot is not set
        if keyword_set(first_time) and NOT keyword_set(no_plot) then $
           window, 6, Title='SSG Fitting Window'
        first_time = 0

        CATCH, err
        if err ne 0 then begin
           CATCH, /CANCEL
           message, /NONAME, !error_state.msg, /CONTINUE
           message, 'skipping ' + full_fnames[inday] + ' nday = ' + strtrim(ndays[inday], 2) , /CONTINUE
           CONTINUE
        endif

        ssg_fit1spec, ndays[inday], autofit=(nndays gt 1), no_plot=no_plot, _EXTRA=extra

     endfor ;; each spectrum to be fitted

     ;; ssg_select returns -1 when nothing is selected, but if more
     ;; than one spectrum is selected a multi-element ndays confuses
     ;; the until test
     if nndays gt 1 then $
        ndays = 0
  endrep until ndays eq -1 or keyword_set(non_interactive)
  
end

