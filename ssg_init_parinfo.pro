; +
; $Id: ssg_init_parinfo.pro,v 1.2 2015/03/04 15:59:55 jpmorgen Exp $

; sse_init_parinfo.pro  Make num blank parinfo records appropriate for
; the Io SSE fitting system.  If optional parameter voigt is
; specified, use its value(s) to initialize four parinfo records
; appropriate for a Voigt.  

; We want to build a structure that is usable direcly with mpfitfn,
; but can also be used in via some intermediate routines that take
; care of trivial calculations like Doppler shifts.  It seems like the
; parinfo structure is the natural place for this.  So far, Craig has
; defined:

;     .VALUE - the starting parameter value (I use params instead)
;     .FIXED - a boolean value;  
;     .LIMITED - a two-element boolean array.
;     .LIMITS - a two-element float or double array
;     .PARNAME - a string, giving the name of the parameter.
;     .STEP - the step size for numerical derivatives.  0=autodetect
;     .MPSIDE - the sidedness of the finite difference
;     .MPMAXSTEP - the maximum change in the parameter value per iter.
;     .TIED - e.g. : parinfo(2).tied = '2 * P(1)'.
;     .MPPRINT - if set to 1, then the default ITERPROC will print

; Since we are lumping all kinds of different parameters together, we
; need to have some quick way of identifying and separating things
; out.  Define some additional fields in the structure (some integer,
; some floating point) that act as handles for the where() function.
; There is intentionally some overlap between tags to make for easy
; referencing in a variety of circumstances.
;
;	.sseID -- high-level parameter type identification.  These are
;                 meant to be independent of the actual functional
;                 forms used.  This tag should be set for each
;                 parameter that composes a function (i.e. for all 4
;                 Voigt parameters).
;		0 = not an SSE parameter
;		1 = dispersion relation
;		2 = sensitivity function (response to white light source)
; 		3 = instrument profile (response to delta function)
;		4 = doppler shift (e.g. reflected solar or object)
;		5 = continuum
;		6 = line
;	.sseftype -- function type identification, in generic terms.
;                    The idea is not to get too fancy here, just
;                    define a few basic types so the parameters can be
;                    grabbed easily by the primitives that define
;                    these functions.  The meanings of the parameters
;                    themselves are kept vague at this point (e.g. the
;                    parameter "related to area" might end up being
;                    equivalent width, as determined by .sseunit;
;                    "related to line center" might be a delta
;                    wavelength from the expected value if .sseowl is
;                    set).
;		0 = not an SSE parameter
;		1 = polynomial.  Let's try to make a generic
;		    definition for a polynomial function made up of
;		    segments.  The first segment starts at the [pixel]
;		    value labeled with 1.0 (0 if there is no 1.0) and
;		    the segments proceed to the right.  The
;		    polynomial coeficients are labeled 1.x0-1.x999,
;		    where x >= 1.  The boundary between segment 1.x
;		    and 1.(x+1) is labeled 1.0x.  If you need more
;		    than 9 segments, make another ftype.
;		  1.01-1.09 = 1st through 9th reference values
;		        (e.g. for dispersion, reference pixel for zero
;		        point of wavelength axis; for sensitivity
;		        function, first break point)
;		  1.10-1.1999 = 0-999th order polynomial coefficient
;		        of first polynomial
;		  1.20-1.2999 = 0-999th order polynomial coefficient
;		        of second polynomial
;		  1.30-1.n999 = etc.
;		2 = delta fn
;		  2.1 = related to line center
;		  2.2 = related to area
; 		3 = Gaussian
;		  3.1 = related to line center
;		  3.2 = related to area
;		  3.3 = related to width
; 		4 = Voigt
;		  4.1 = related to line center
;		  4.2 = related to area
;		  4.3 = related to Gaussian width
;		  4.4 = related to Lorentzian width
; 	.sseunit -- The units of the parameter in question in a very
;                   generic sense.  This is sort of a bitmap, where
;                   multiplication of units means adding their tag
;                   values, and division means subtracting.  So a
;                   first-order dispersion coefficient will have a tag
;                   value of 2, corresponding to, e.g. A/pix.  So as
;                   not to be too confusing, polynomial coefs past 1st
;                   order have the same label as 1st order, since their
;                   actual units are easy to figure out at that point.
;                   This is mainly to get a handle on whether this is
;                   a pre- or post- dispersion relation function and
;                   whether or not the polynomial is scaled (scaling
;                   parameters all to approximately the same value
;                   makes them easier to display and makes mpfit
;                   behave better without having to fiddle with the
;                   .*step tags).
;		0 = units not specified
;		+/-1 = instrument coordinates (e.g. pixels)
;		+/-3 = converted coordinates (e.g. Angstroms, km/s,
;		       electrons/s) 
;		+/-10 = if for a polynomial coef, scaled by a factor
;		        of 10 per order (+100 would be scaled by a
;		        factor of 100)

;	.ssesrc -- source of the feature.  This is a generic list
;                  arranged in increasing distance from the detector.
;                  Non-conflicting decimal subcategories should be
;                  added as necessary so that an encyclopedic line
;                  list can be kept organized.  This tag, together
;                  with sserwl should be sufficient for determining
;                  the unique
;		0 = source not identified
;		1 = instrument
;		2 = telescope
;		3 = anthropogenic (e.g. city lights)
;		4 = terrestrial atmospheric
;		 4.1 = narrow absorption fearures
;		   4.11 = O2
;		   4.12 = H2O
;		 4.2-4.3 increasingly broad absorption features
;		 4.4 = Extinction coefficients
;		 4.5 = narrow emission features
;		 4.6-4.9 = increasingly broad emission features
;		5 = zodiacal
;		6 = object
;		7 = reflected sunlight from object
;		 7.1 = Well-behaved solar Fraunhoffer features
;		       fittable with Voigt profiles
;		8 = background light
;	.sserwl -- rest wavelength of the line, or rather, the
;                  wavelength of the line when the source is viewed
;                  directly by an observer traveling at the same speed
;                  as the source (i.e. solar lines are
;                  relativistically shifted).  This, together with
;                  ssesrc is the handle by which lines can be uniquely
;                  identified.  Both tags should be set to the same
;                  values on all of the parameters that define a
;                  particular feature.
;	.ssedop -- Doppler group.  For ease of handling, only set this
;                  for the line center or other appropriate parameter
;                  in the function.
;		0 = parameter not Doppler shiftable
;		1 = single Doppler shift (e.g. Io/object emission
;		    line)
;		2 = double Doppler shift (e.g. reflected sunlight)
;		>2 = other Doppler shift situation applies
;		     (e.g. multiple Galilean satellites in view,
;		     multiple IPM/ISM clouds at different velocities)
;	.sseowl -- observed (i.e. Doppler shifted) wavelength.  If
;                  this tag is non-zero, the parameter it corresponds
;                  to is assumed to be the OFFSET from this value in
;                  the same units.
;	.ssevalue -- best a priori estimate of parameter value
;	.sselink -- for emulating broad lines with multiple Voigts or
;                   for multiplets with fixed ratios.  The catalog(s)
;                   will have these in absolute form (e.g. referenced
;                   to the parameter number in that catalog), this is
;                   for the actual parameter list we have constructed.
;                   This follows the syntax of .LINK and, unlike the
;                   .sse* parameters above, is specific to each Voigt
;                   parameter (e.g. you can link widths, etc.)  This
;                   will be painful to implement properly when lines
;                   are removed

; -

function sse_init_parinfo, num, voigt=voigt

  if N_params() eq 0 then num = 1
  if num lt 1 then return, 0

  if NOT keyword_set(voigt) then begin
     
     parinfo = { fixed:0, $
                 limited:[0,0], $
                 limits:[0.D,0.D], $
                 parname:'', $
                 tied:'', $
                 vfID:0, $
                 sseID:0, $
                 ssegroupID:0, $
                 sserwl:0D, $
                 sseowl:0D, $
                 ssedop:0, $
                 ssevalue: !values.d_nan, $
                 sselink:'' }

     return, replicate(parinfo, num)

  endif  ;; Just num blank records

  ;; Fancier structure (just Voigt for now)
  if num gt 1 then message, 'ERROR: you can only intialize one Voigt at a time'

  ;; voigt = [rwl, equiv width, dop width, lor width].  Extend if
  ;; necessary
  while N_elements(voigt) lt 4 do voigt = [voigt, 0d]
  ;; Generic defaults for widths and ews
  if voigt[1] eq 0 then voigt[1] = 1. ; milliAngstroms
  if voigt[2] eq 0 then voigt[2] = 0.1 ; Angtroms
  if voigt[3] eq 0 then voigt[3] = 0 ; Turn off Lorentzian

  ;; Make tokens for everything
  vfid_cont = 1
  vfid_center = 2
  vfid_area = 3
  vfid_ew = 3
  vfid_dop = 4
  vfid_lor = 5
  vfid_first = vfid_center
  vfid_last = vfid_lor

  sseid_disp = 1
  sseid_dop = 2
  sseid_cont = 3
  sseid_voigt = 4

  ;; Initializing 4 parinfo records for a Voigt.  
  parinfo = sse_init_parinfo(4)

  ;; Important step: save off the rest wavelegnth to parinfo.sserwl
  ;; for all parameters, but put the observed wavelength (which may
  ;; eventually be Doppler shifted) into the .sseowl field of ONLY the
  ;; line center parameter.  That way Doppler shifts can be handled
  ;; without explicit knowledge of the function
  parinfo[*].sserwl = voigt[0]
  parinfo[0].sseowl = voigt[0]

  lname = string(format='(f9.4)', voigt[0])
  ;; Now set the wavelength to 0 since we will be fitting it as
  ;; delta wavelength 
  voigt[0] = 0.D

  vpnames = ['Dlambda', 'EW', 'DopFWHM', 'LorFWHM']
  for ip = 0,3 do begin
     ;; Set limits to initial values so mpfit won't complain when it
     ;; starts (even if limited turned off).  This is sort of a pain,
     ;; but doesn't constrain things later on in the fit.  Just
     ;; remember to reset parinfo.limits whenever you turn around and
     ;; do another fit.
     parinfo[ip].limits = [voigt[ip], voigt[ip]]
     parinfo[ip].parname = string(lname, ' ', vpnames[ip])
     parinfo[ip].vfID = ip + 2
  endfor
  parinfo.sseID[*] = sseid_voigt

  return, parinfo

end
