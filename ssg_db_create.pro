;+
; $Id: ssg_db_create.pro,v 1.5 2002/12/06 17:29:31 jpmorgen Exp $

; ssg_db_create  Creates the SSG database structures from scratch.
; Hopfully this won't be used very often!  If necessary, however, this
; code can me modified to create a new set of .dbd files, and still
; other code adapted from this to copy existing entries into the new
; database 

; ERASEDBD = allow rewriting of DBD.  NO CHECKING FOR VALIDITY OF
; EXISTING DATABASE IS DONE.  So if you change the structure of the
; database, you should specify ERASEDATA too.  On the other hand, you
; can just do ERASEDBD to change some trivial formatting of dbprint

;-

pro ssg_db_create, outdir, ERASEDBD=newdbd, ERASEDATA=newdb, NEWINDEX=newindex

  ON_ERROR, 2
  if NOT keyword_set(outdir) then message, 'ERROR: supply directory into which database descriptors will be written'

  cd, outdir
  
  nf=3
  files=strarr(nf)
  files[0] = 'ssg_reduce'
  files[1] = 'oi_6300_fit'
  files[2] = 'io_oi_analyze'
  for i=0,nf-1 do begin
     tmp=findfile(string(files[i],'.dbd'), COUNT=count)
     if count gt 0 and NOT keyword_set(newdbd) then $
       message, 'ERROR: will not overwrite '+ tmp+' DataBase Descriptor file unless /ERASEDBD is specified.  If you are just making changes to the formats, it is OK to /ERASEDBD.  The software you are using to create the DBD file should be stored in a revision control system so that you can track changes and get back  to a version that works with a particular binary database (e.g. everything has to line up in the database).'
     tmp=findfile(string(files[i],'.dbx'), COUNT=count)
     if count eq 0 then newindex=1 else begin
        if NOT keyword_set(newindex) then begin
           newindex = 0
           message, /CONTINUE, 'NOTE: Preserving index file ' +tmp
        endif
     endelse

     tmp=findfile(string(files[i],'.dbf'), COUNT=count)
     if count eq 0 then newdb=1 else begin
        if NOT keyword_set(newdb) then begin
           newdb = 0
           message, /CONTINUE, 'NOTE: Preserving database file ' +tmp
        endif
     endelse
  endfor

  ;; REDUCTION DATABASE
  openw, lun, string(files[0], '.dbd'), /get_lun
  printf, lun, '#title'
  printf, lun, 'MMP SSG Reduction Database'
  printf, lun, ' '
  printf, lun, '#maxentries'
  printf, lun, '6000'
  printf, lun, ' '
  printf, lun, '#items'
  ;; WARNING if you change any code between here and nday, make sure
  ;; you modify ssg_db_init too.
  printf, lun, 'raw_dir		C*200		directory containing raw file'
  printf, lun, 'raw_fname	C*80		raw file name (blank for products such as median bias frames)'
  printf, lun, 'dir		C*200		directory containing reduced file'
  printf, lun, 'fname		C*80		reduced file name'
  printf, lun, 'object		C*80		object name'
  printf, lun, 'imagetype	C*80		image type (object, flat, bias, etc.)'
  printf, lun, 'date		C*10		yyyy-mm-dd'
  printf, lun, 'time		C*8		hh:mm:ss (start:UT)'
  printf, lun, 'exptime		R*4             exposure time (seconds)'
  printf, lun, 'dark		R*4             dark time (seconds)'
  printf, lun, 'detector	C*80		detector name (ti4 or ti5)'
  printf, lun, 'hgain		R*4		recorded gain, electrons per adu'
  printf, lun, 'dwell		I*2		sample integration time, microseconds'
  printf, lun, 'hrdnoise	R*4		recorded read noise, electrons'
  printf, lun, 'camtemp		I*2		camera temperature, Celsius'
  printf, lun, 'dewtemp		I*2		dewar temperature, Celsius'
  printf, lun, 'db_date		C*10		database update date (yyyy-mm-dd UT)'
  printf, lun, 'nday		R*8		decimal day since 1990-Jan-01 (midpoint)'
  printf, lun, 'typecode	B*1		0=bias,1=dark,2=comp,3=flat,4=sky flat, 5=object'
  ;; I'd like to make these B*2, but dbext translates that into a 1
  ;; byte variable.
  printf, lun, 'bad		I*2		bitmap'
; 16384=unspecified (e.g. bad slicer shape or diseprsion for the whole night)
; 8192=overclock
; 4096=slicer center
; 2048=camera rotation
; 1024=wavelen scale gap
; 512=spectral extraction
; ???=slicer shape
; 128=IP
; 64=extraction, maybe these should be negative....=solar fitting, 32=narrow atm fitting, 16=broad atm fitting, 8=airglow overlap, 4=Io [OI] velocity wrong, 2=width wrong, 1=large error bar, 0=good'
  printf, lun, 'proc_flag	I*2		processing progress flag, 0=nothing';, 1=
  printf, lun, 'gain		R*4		derived gain, electrons per adu'
  printf, lun, 'rdnoise		R*4		derived read noise, electrons'
  printf, lun, 'n_ovrclk	I*2		number of overclock rows'
  printf, lun, 'med_ovrclk(800) R*4		median overclock spectrum'
  printf, lun, 'av_ovrclk(800)	R*4		average overclock spectrum, CR removed'
  printf, lun, 'pred_ovrclk(800) R*4		predicted overclock spectrum'
  printf, lun, 'med_bias	R*4		median bias value from overclock region'
  printf, lun, 'av_bias		R*4		average bias value from overclock region'
  printf, lun, 'stdev_bias	R*4		stdev bias value from overclock region'
  printf, lun, 'pred_bias(10)	R*4		10th order polynomial describing time variation of bias x0=nday'
  printf, lun, 'pred_bias0	R*4		predicted bias at , x0=fix(nday,0)'
  printf, lun, 'bias_fname	C*80		bias image associated with this file'
  printf, lun, 'dark_fname	C*80		dark image associated with this file'
  printf, lun, 'flat_fname	C*80		flatfield image associated with this file'
  printf, lun, 'm_sli_cent	R*4		Measured slicer central pixel in cross-disp direction.'
  printf, lun, 'sli_cent	R*4		Predicted slicer central pixel in cross-disp direction.'
  printf, lun, 'm_cam_rot	R*4		Measured camera rotation: clockwise deviation of flatfield pattern on CCD.'
  printf, lun, 'cam_rot		R*4		Predicted camera rotation based on fit'
  printf, lun, 'm_slice(100)	R*4		10x10 array of polynomial coefsexpressing measured spectral variation of slicer shape.';  First row is a 10th order polynomial description of a slice at the midpoint of the spectrum.  Columns are polynomials in dispersion pixel number describing the spectral change in the slicer shape.'
  printf, lun, 'slice(100)	R*4		Predicted slicer shape for this image'
  printf, lun, 'flat_cut	R*4		flatfield value below which all pixels are set to 0'
  printf, lun, 'cut_val		R*4		Using template image, sigma value above which pixel is assumed to be a cosmic ray hit'
  printf, lun, 'ncr		R*4		estimated number of cosmic ray hits inside flatcut'
  printf, lun, 'nbad		I*2		number of bad pixels (from CR and bad cols) inside flatcut'
  printf, lun, 'm_IP(100)	R*4		10x10 array describing instrument profile.  Like slice shape, reference is midpoint'
  printf, lun, 'IP(100)		R*4		Predicted IP for this spectrum'
  printf, lun, 'm_dispers(10)	R*4		up to 10 polynomial coefficients describing measured dispersion relation.  Ref to  midpoint '
  printf, lun, 'dispers(10)	R*4		individually fit dispersion (starts with time varying fit to m_dispers'
  printf, lun, 'wavelen(800)	R*4		wavelength axis (angstroms), no offset'
  printf, lun, 'spectrum(800)	R*4		bias subtracted, flatfielded, derotated, CR rejected spectrum'
  printf, lun, 'spec_err(800)	R*4		statistical error on spectrum'
  printf, lun, 'cross_disp(400)	R*4		bias subtracted, flatfielded, derotated, CR rejected cross-dispersion spectrum'
  printf, lun, 'cross_err(400)	R*4		statistical error on cross dispersion spectrum'
  printf, lun, 'med_spec	R*4		median spectral value'
  printf, lun, 'av_spec		R*4		average spectral value'
  printf, lun, 'min_spec	R*4		minimum spectral value'
  printf, lun, 'max_spec	R*4		maximum spectral value'
  printf, lun, 'med_cross	R*4		median cross dispersion value'
  printf, lun, 'av_cross	R*4		average cross dispersion value'
  printf, lun, 'min_cross	R*4		minimum cross dispersion value'
  printf, lun, 'max_cross	R*4		maximum cross dispersion value'
  printf, lun, 'bias_vers	B*1		bias subtraction version 0=only try so far'
  printf, lun, 'flat_vers	B*1		flatfield version'
  printf, lun, 'rot_vers	B*1		rotation version'
  printf, lun, 'slice_vers	B*1		slicer shape version'
  printf, lun, 'ip_vers		B*1		IP version'
  printf, lun, 'cr_vers		B*1		CR rejection version'
  printf, lun, 'spec_vers	B*1		spectrum version (number of times spectrum has been extracted, 0=only try so far)'
  printf, lun, 'reduce_comment	C*240		comments on reduction process'
  printf, lun, 'exceptions	C*240		IDL commands separated by & to tweak file info,'

  printf, lun, ' '
  printf, lun, '#formats'
  printf, lun, 'raw_dir		A80		raw,directory,name'
  printf, lun, 'raw_fname	A20		raw,file,name'
  printf, lun, 'dir		A80		reduced,directory,name'
  printf, lun, 'fname		A20		reduced,file,name'
  printf, lun, 'object		A20		object,name'
  printf, lun, 'imagetype	A10		image,type'
  printf, lun, 'date		A11		DATE,YYYY-MM-DD,'
  printf, lun, 'exp		F7.3 		EXPO,TIME,SEC'
  printf, lun, 'dark		F7.3		dark,TIME,SEC'
  printf, lun, 'detector	A80		detector,name'
  printf, lun, 'hgain		F7.3		recorded,gain,e_/DN'
  printf, lun, 'dwell		I4		samp,time,us'
  printf, lun, 'hrdnoise	F7.3		header,rdnoise,e_'
  printf, lun, 'camtemp		I4		cam,temp,C'
  printf, lun, 'dewtemp		I4		dew,temp,C'
  printf, lun, 'db_date		A11		DATABASE,UPDATE,YYYY-MM-DD'
  printf, lun, 'nday		F11.5		DECIMAL,DAY,>90/01/01'
  printf, lun, 'typecode	I4		0b1di,2c3f,4s5o'
  printf, lun, 'bad		I4		0,is,good'
  printf, lun, 'gain		F7.3		calc,gain,e_/DN'
  printf, lun, 'rdnoise		F7.3		calc,rdnoise,e_'
  printf, lun, 'n_ovrclk	I4		novr,clck,rows'
  printf, lun, 'med_ovrclk	F7.3		median,overclk,spec'
  printf, lun, 'av_ovrclk	F7.3		avovrc,spec,no_CR'
  printf, lun, 'pred_ovrclk	F7.3		predic,overclk,spec'
  printf, lun, 'med_bias	F7.3		median,ovrclk,value'
  printf, lun, 'av_bias		F7.3		averag,ovrclk,value'
  printf, lun, 'stdev_bias	F7.3		stdev,ovrclk,value'
  printf, lun, 'pred_bias	F7.3		predic,ovrclk,value'
  printf, lun, 'pred_bias0	F7.3		predic,bval,nday.0'
  printf, lun, 'bias_fname	A80		bias,reference,file'
  printf, lun, 'dark_fname	A80		dark,reference,file'
  printf, lun, 'flat_fname	A80		flatfield,reference,file'
  printf, lun, 'm_sli_cent	F7.3		measred,slicer,center'
  printf, lun, 'sli_cent	F7.3		predict,slicer,center'
  printf, lun, 'm_cam_rot	F7.3		measred,cam_rot,clockws'
  printf, lun, 'cam_rot		F7.3		predict,cam_rot,clockws'
  printf, lun, 'm_slice		F7.3		measred,slicer,shape'
  printf, lun, 'slice		F7.3		pred,slicer,shape'
  printf, lun, 'flat_cut	F7.3		flat,cut,4trim'
  printf, lun, 'cut_val		F7.3		cosmic,ray,cutval'
  printf, lun, 'ncr		F7.3		est,number,CR'
  printf, lun, 'm_IP		F7.3		measred,inst,prof'
  printf, lun, 'IP		F7.3		pred,inst,prof'
  printf, lun, 'm_dispers	F9.3		measred,dispers,relate'
  printf, lun, 'dispers		F9.3		dispers,relate,poly'
  printf, lun, 'spectrum	F7.3		spec,flat,no_CR'
  printf, lun, 'spec_err	F7.3		spec,stat,err'
  printf, lun, 'cross_disp	F7.3		cross,disp,spec'
  printf, lun, 'cross_err	F7.3		cross,disp,err'
  printf, lun, 'med_spec	F7.3		median,spec,value'
  printf, lun, 'av_spec		F7.3		averag,spec,value'
  printf, lun, 'min_spec	F7.3		min,spec,value'
  printf, lun, 'max_spec	F7.3		max,spec,value'
  printf, lun, 'med_cross	F7.3		median,cross,value'
  printf, lun, 'av_cross	F7.3		averag,cross,value'
  printf, lun, 'min_cross	F7.3		min,cross,value'
  printf, lun, 'max_cross	F7.3		max,cross,value'
  printf, lun, 'bias_vers	I4		bias,vers,'
  printf, lun, 'flat_vers	I4		flat,vers,'
  printf, lun, 'rot_vers	I4		rot,vers,'
  printf, lun, 'slice_vers	I4		slice,vers,'
  printf, lun, 'ip_vers		I4		IP,vers,'
  printf, lun, 'cr_vers		I4		CR,rej,vers'
  printf, lun, 'spec_vers	I4		spec,vers'
  printf, lun, 'reduce_comment	A240		comment,reduction,process'
  printf, lun, 'exceptions	A240		IDL,extra_commands,'

  printf, lun, ' '
  printf, lun, '#index'
  printf, lun, 'nday       	sort/index'
;  printf, lun, 'typecode   	sort/index'
  printf, lun, 'gain	   	sort/index'
  printf, lun, 'rdnoise	   	sort/index'
  printf, lun, 'n_ovrclk	sort/index'
  printf, lun, 'med_bias	sort/index'
  printf, lun, 'av_bias		sort/index'
  printf, lun, 'pred_bias0	sort/index'
;  printf, lun, 'camera_rot	sort/index'
  printf, lun, 'flat_cut	sort/index'
  printf, lun, 'cut_val		sort/index'
  printf, lun, 'ncr		sort/index'
  printf, lun, 'nbad		sort/index'
  printf, lun, 'med_spec	sort/index'
  printf, lun, 'av_spec		sort/index'
;  printf, lun, 'min_spec	sort/index'
  printf, lun, 'max_spec	sort/index'
  printf, lun, 'med_cross	sort/index'
  printf, lun, 'av_cross	sort/index'
;  printf, lun, 'min_cross	sort/index'
  printf, lun, 'max_cross	sort/index'
;  printf, lun, 'bias_vers	sort/index'
;  printf, lun, 'flat_vers	sort/index'
;  printf, lun, 'rot_vers	sort/index'
;  printf, lun, 'slice_vers	sort/index'
;  printf, lun, 'ip_vers		sort/index'
;  printf, lun, 'cr_vers		sort/index'
;  printf, lun, 'spec_vers	sort/index'

  ;; Unfortunately, one pointer cannot point to more than one
  ;; database.  If I really need to reference back, I'll have to
  ;; fiddle
  printf, lun, ' '
  printf, lun, '#pointers'
  printf, lun, 'nday              ', files[1]

  close, lun
  free_lun, lun

  ;; FITTING DATABASE
  openw, lun, string(files[1], '.dbd'), /get_lun
  printf, lun, '#title'
  printf, lun, 'Io [OI] Spectral Fitting Database'
  printf, lun, ' '
  printf, lun, '#maxentries'
  printf, lun, '6000'
  printf, lun, ' '
  printf, lun, '#items'
  printf, lun, 'nday		R*8		decimal day since 1990-Jan-01 (midpoint)'
  printf, lun, 'new_spec	I*2		Flag indicating a new spectrum has been generated since last fit'
  printf, lun, 'bad		I*2		bitmap'
  printf, lun, 'm_dispers(10)	R*4		up to 10 polynomial coefficients describing measured dispersion relation.  Ref to  midpoint '
  printf, lun, 'dispers(10)	R*4		individually fit dispersion (starts with time varying fit to m_dispers'
   printf, lun, 'wavelen(800)	R*4		wavelength axis (angstroms), no offset'
   printf, lun, 'spectrum(800)	R*4		bias subtracted, flatfielded, derotated, CR rejected spectrum'
   printf, lun, 'spec_err(800)	R*4		statistical error on spectrum'
   printf, lun, 'cross_disp(400)	R*4		bias subtracted, flatfielded, derotated, CR rejected cross-dispersion spectrum'
   printf, lun, 'cross_err(400)	R*4		statistical error on cross dispersion spectrum'
   printf, lun, 'med_spec	R*4		median spectral value'
   printf, lun, 'av_spec		R*4		average spectral value'
   printf, lun, 'min_spec	R*4		minimum spectral value'
   printf, lun, 'max_spec	R*4		maximum spectral value'
   printf, lun, 'med_cross	R*4		median cross dispersion value'
   printf, lun, 'av_cross	R*4		average cross dispersion value'
   printf, lun, 'min_cross	R*4		minimum cross dispersion value'
   printf, lun, 'max_cross	R*4		maximum cross dispersion value'
   printf, lun, 'm_IP(100)	R*4		10x10 array describing instrument profile.  Like slice shape, reference is midpoint'
   printf, lun, 'IP(100)		R*4		Predicted IP for this spectrum'
   printf, lun, 'fit_date	C*10		database update date (yyyy-mm-dd)'
   printf, lun, 'nfree		I*2		number of free parameters'
   printf, lun, 'chisq		R*8             chi^2'
   printf, lun, 'redchisq	R*8             reduced chi^2'
   printf, lun, 'value(300)	R*8             value       '
   printf, lun, 'fixed(300)	R*8             fixed       '
   printf, lun, 'limited(300)	R*8             limited     '    
   printf, lun, 'limits(300)	R*8             limits      '    
   printf, lun, 'parname(300)	C*20		parname     '    
   printf, lun, 'tied(300)	C*80		tied        '    
   printf, lun, 'vfID(300)	I*2             vfID        '    
   printf, lun, 'ssgID(300)	I*2             ssgID       '    
   printf, lun, 'ssggroupID(300)	I*2             ssggroupID  '    
   printf, lun, 'ssgrwl(300)	R*8             ssgrwl      '    
   printf, lun, 'ssgowl(300)	R*8             ssgowl      '    
   printf, lun, 'ssglink(300)	C*80		ssglink     '    
   printf, lun, 'ssgdop(300)	I*2		ssgdop      '    
   printf, lun, 'model_spec(800)	R*4		model spectrum'
   printf, lun, 'mod_stat_err(800) R*4		statistical error bars on model spectrum'
  printf, lun, 'weighting(800)	R*4		this spectrum multiplies the error bars used for the fit so points far from the line of interest are weighted less'
  printf, lun, 'cen_weight	R*4		center pixel of weighting function, 0=not a simple V-shaped function'
  printf, lun, 'med_weight	R*4		median of weighting function'
  printf, lun, 'av_weight	R*4		average of weighting function'
  printf, lun, 'min_weight	R*4		min of weighting function'
  printf, lun, 'max_weight	R*4		max of weighting function'
  printf, lun, 'weight_vers	B*1		weighting function version'
  printf, lun, 'fit_vers	B*1		fit version'


  printf, lun, ' '
  printf, lun, '#formats'
  printf, lun, 'nday		F11.5		DECIMAL,DAY,>90/01/01'


  printf, lun, ' '
  printf, lun, '#index'
  printf, lun, 'nday		sort/index'
  printf, lun, 'med_spec	sort/index'
  printf, lun, 'av_spec		sort/index'
  printf, lun, 'min_spec	sort/index'
  printf, lun, 'max_spec	sort/index'
  printf, lun, 'med_cross	sort/index'
  printf, lun, 'av_cross	sort/index'
  printf, lun, 'min_cross	sort/index'
  printf, lun, 'max_cross	sort/index'
  printf, lun, 'fit_vers	sort/index'
  printf, lun, 'nfree		sort/index'
  printf, lun, 'chisq		sort/index'
  printf, lun, 'cen_weight	sort/index'
  printf, lun, 'med_weight	sort/index'
  printf, lun, 'av_weight	sort/index'
  printf, lun, 'min_weight	sort/index'
  printf, lun, 'max_weight	sort/index'
  printf, lun, 'weight_vers	sort/index'
  printf, lun, 'fit_vers	sort/index'

  ;; Unfortunately, one pointer cannot point to more than one
  ;; database.  If I really need to reference back, I'll have to
  ;; fiddle
  printf, lun, ' '
  printf, lun, '#pointers'
  printf, lun, 'nday              ', files[0]

  close, lun
  free_lun, lun

  ;; ANALYSIS DATABASE
  ;; This is basically Melanie's database right now.  I will probably
  ;; want to add additional columns to handle other datasets that I
  ;; will be fiddling with
  openw, lun, string(files[2], '.dbd'), /get_lun
  printf, lun, '#title'
  printf, lun, 'Io [OI] Analysis Database'
  printf, lun, ' '
  printf, lun, '#maxentries'
  printf, lun, '6000'

  printf, lun, '#items'
  printf, lun, 'date              C*10         yyyy-mm-dd'
  printf, lun, 'time              C*8          hh:mm:ss (start:UT)'
  printf, lun, 'nday              R*8          decimal day since 1990-Jan-01 (midpoint)'
  printf, lun, 'object            C*13         object name'
  printf, lun, 'obj_code          B*1          object code (Jup=0,Io=1,Europa=2,Ganymede=3,Callst=4,Sky=5)'
  printf, lun, 'line              C*7          line of observation ([O I],Na D,H-alpha(6563A))   '
  printf, lun, 'lambda            I*2          wavelength (Angstroms) (e.g.,[O I] 6300, Na D 5893)'
  printf, lun, 'irafname          C*16         name of original iraf file'
  printf, lun, 'specname          C*25         name of extracted spectrum file'
  printf, lun, 'exptime           R*4          exposure time (seconds)'
  printf, lun, 'zd                R*4          zenith distance (midpoint:degrees)'
  printf, lun, 'am                R*4          air mass (midpoint)'
  printf, lun, 'ha                R*4          hour angle (midpoint:decimal)'
  printf, lun, 'side              C*4          east/west'
  printf, lun, 'phi               R*4          Io(moon) orbital phase (midpoint:degrees)'
  printf, lun, 'long_3            R*4          Io longitude III (midpoint:degrees)'
  printf, lun, 'lat_sub           R*4          sub-latitude of Io(moon) center from the observer (degrees)'
  printf, lun, 'lat_mag           R*4          magnetic latitude (midpoint:degrees)'
  printf, lun, 'spa               R*4          solar phase angle (midpoint:degrees)'
  printf, lun, 'jnp               R*4          Jovian north pole position angle (deg)'
  printf, lun, 'lshell            R*4          Magnetic L-shell at Io (Jovian Radii)(offset tilted dipole)'
  printf, lun, 'b_field           R*4          Magnetic field strength (nT)'
  printf, lun, 'io_lat            R*4          Iocentric latitude of B-field tangent point (degrees)'
  printf, lun, 'io_dia            R*4          Io(moon) diameter (arcsec)'
  printf, lun, 'delta             R*8          geocentric distance (AU)'
  printf, lun, 'r                 R*8          heliocentric distance (AU)'
  printf, lun, 'deldot            R*4          geocentric velocity (midpoint:km/s)'
  printf, lun, 'rdot              R*4          heliocentric velocity (midpoint:km/s)'
  printf, lun, 'dice              R*4          distance Io-centrifugal equator (midpt:Jovian Radii)'
  printf, lun, 'deldot_m          R*4          measured geocentric velocity (km/s)'
  printf, lun, 'err_deldot_m      R*4          error in measured geocentric velocity (km/s)'
  printf, lun, 'fline             R*4          measured line flux (electrons/sec)'
  printf, lun, 'err_fline         R*4          error in line flux (electrons/sec)'
  printf, lun, 'fcont             R*4          measured continuum flux (electrons/sec/Angstrom)'
  printf, lun, 'err_fcont         R*4          error in continuum flux (electrons/sec/Angstrom)'
  printf, lun, 'wc                R*4          measured convolved FWHM (Angstroms)'
  printf, lun, 'err_wc            R*4          error in convolved FWHM (Angstroms)'
  printf, lun, 'wd                R*4          deconvolved FWHM (Angstroms)'
  printf, lun, 'err_wd            R*4          error in deconvolved FWHM (Angstroms)'
  printf, lun, 'weq               R*4          equivalent width (Angstroms)'
  printf, lun, 'err_weq           R*4          error in equivalent width (Angstroms)'
  printf, lun, 'alf               R*4          apparent line flux (photons/sec/cm^2)'
  printf, lun, 'err_alf           R*4          error in apparent line flux (photons/sec/cm^2)'
  printf, lun, 'ag_flux           R*4          airglow line flux (electrons/sec)'
  printf, lun, 'err_ag_flux       R*4          error in airglow line flux (electrons/sec)'
  printf, lun, 'redchisq          R*4          reduced chi-squared        '
  printf, lun, 'freeparam         I*2          number of free parameters in the model'
  printf, lun, 'numlines          I*2          number of spectral lines used in the model'
  printf, lun, 'rows(150)         L*1          rows extracted (0=false, 1=true)'
  printf, lun, 'nrows             I*2          number of rows extracted'
  printf, lun, 'npix              I*2          number of pixels extracted'
  printf, lun, 'p_date            C*10         processing date (yyyy-mm-dd)'
  printf, lun, 'quality           C*7          quality assessment flag'
  printf, lun, 'ip                R*4          instrumental profile (FWHM: Angstroms)'
  printf, lun, 'disp              R*4          dispersion (Angstroms/pixel)'
  printf, lun, 'refpix            R*4          nominal reference pixel'
  printf, lun, 'refwave           R*4          nominal reference wavelength (Angstroms)'
  printf, lun, 'comp_min          R*4          comp lamp FWHM minimum (Angstroms)'
  printf, lun, 'comp_max          R*4          comp lamp FWHM maximum (Angstroms)'
  printf, lun, 'vers              C*4          model version'
  printf, lun, 'db_date           C*10         database update date (yyyy-mm-dd)'
  printf, lun, 'intensity         R*4          surface brightness (alf/area of disk of Io) (kRaleighs)'
  printf, lun, 'err_intensity     R*4          error in surface brightness (kRaleighs)'
  printf, lun, 'lat_subsolar      R*4          sub-latitude of Io(moon) center from the Sun (degrees)'
  printf, lun, 'long_subsolar     R*4          sub-longitude of Io(moon) center from the Sun (degrees)'
  printf, lun, 'ra                R*4          astrometric right ascension J2000 (degrees)'
  printf, lun, 'dec               R*4          astrometric declination J2000 (degrees)'
  printf, lun, 'spares(76)        L*1          future use'
  printf, lun, ''
  printf, lun, '#formats'
  printf, lun, 'date              A11          DATE,YYYY-MM-DD,'
  printf, lun, 'time              A9           TIME,HH:MM:SS,UT'
  printf, lun, 'nday              F11.5        DECIMAL,DAY,>90/01/01'
  printf, lun, 'object            A13           ,OBJECT_NAME'
  printf, lun, 'obj_code          I4           OBJ,CODE'
  printf, lun, 'line              A7            ,LINE'
  printf, lun, 'lambda            I6           WAVE-,LENGTH,A'
  printf, lun, 'irafname          A16           ,IRAF_NAME'
  printf, lun, 'specname          A25          EXTRACTED,SPECTRUM,FILE'
  printf, lun, 'exp               I4           EXPO,TIME,SEC'
  printf, lun, 'zd                F6.3         ZENITH,DIST,DEG'
  printf, lun, 'am                F7.3          ,AIRMASS'
  printf, lun, 'ha                F6.3         HOUR,ANGLE'
  printf, lun, 'side              A5           EAST/,WEST'
  printf, lun, 'phi               F8.2         ORBITAL,PHASE,DEG  '
  printf, lun, 'long_3            F9.2         LONGITUDE,III,DEG'
  printf, lun, 'lat_sub           F8.2         SUB-,LATITUDE,DEG'
  printf, lun, 'lat_mag           F8.2         MAGNETIC,LATITUDE,DEG'
  printf, lun, 'spa               F10.2        SOLAR,PHASE,ANGLE(DEG)'
  printf, lun, 'jnp               F7.2         JOVIAN,N_POLE,POS_ANG'
  printf, lun, 'lshell            F7.2         MAG,L-SHELL,Rj'
  printf, lun, 'b_field           F8.2         FIELD,STRENGTH,nT'
  printf, lun, 'io_lat            F9.2         IOCENTRIC,LATITUDE,DEG'
  printf, lun, 'io_dia            F8.3         IO,DIAMETER,ARCSEC'
  printf, lun, 'delta             F10.8        GEOCENTRIC,DISTANCE,(AU) '
  printf, lun, 'r                 F10.8        HELIO-,CENTRIC,DIST(AU)'
  printf, lun, 'deldot            F10.4        GEOCENTRIC,VELOCITY,(KM/S)'
  printf, lun, 'rdot              F10.4        HELIO-,CENTRIC,VEL(KM/S)'
  printf, lun, 'dice              F8.3         DIST.,IO-CENTR,EQ(Rj)'
  printf, lun, 'deldot_m          F10.4        MEASURED,GEOCENTRIC,VEL(KM/S)'
  printf, lun, 'err_deldot_m      F8.4         ERROR,DELDOT_M,KM/S'
  printf, lun, 'fline             F7.4         LINE,FLUX,e/s       '
  printf, lun, 'err_fline         F7.4         ERROR,FLINE,e/s '
  printf, lun, 'fcont             F9.4         CONTINUUM,FLUX,e/s/A'
  printf, lun, 'err_fcont         F9.4         ERROR,FCONT,e/s/A'
  printf, lun, 'wc                F8.6         FWHM,CONV,A'
  printf, lun, 'err_wc            F8.6         ERROR,WC,A'
  printf, lun, 'wd                F8.6         FWHM,DECONV,A'
  printf, lun, 'err_wd            F8.6         ERROR,WD,A'
  printf, lun, 'weq               F8.6         EQUIVAL,WIDTH,A'
  printf, lun, 'err_weq           F8.6         ERROR,WEQ,A'
  printf, lun, 'alf               F9.6         ABSOLUTE,LINE,FLUX'
  printf, lun, 'err_alf           F9.6         ERROR,ALF'
  printf, lun, 'ag_flux           F8.5         AIRGLOW,FLUX,e/s'
  printf, lun, 'err_ag_flux       F8.5         ERROR,AIRGLOW,FLUX'
  printf, lun, 'redchisq          F7.4         REDUCED,CHI^2'
  printf, lun, 'freeparam         I5           NUM,FREE,PARAM'
  printf, lun, 'numlines          I5           NUM,LINES'
  printf, lun, 'rows              I7           ROWS,0=FALSE,1=TRUE'
  printf, lun, 'nrows             I4           NUM,ROWS,EXT.'
  printf, lun, 'npix              I4           NUM,PIX,EXT.'
  printf, lun, 'p_date            A11          PROCESSING,DATE,YYYY-MM-DD'
  printf, lun, 'quality           A7            ,QUALITY'
  printf, lun, 'ip                F7.4         INSTR.,PROFILE,A'
  printf, lun, 'disp              F8.6         DISPERSN'
  printf, lun, 'refpix            F9.3         REFERENCE,PIXEL'
  printf, lun, 'refwave           F9.3         REFERENCE,WAVE-,LENGTH(A)'
  printf, lun, 'comp_min          F9.2         COMP_LAMP,FWHM,MIN(A)'
  printf, lun, 'comp_max          F9.2         COMP_LAMP,FWHM,MAX(A)'
  printf, lun, 'vers              A7           MODEL,VERSION'
  printf, lun, 'db_date           A11          DATABASE,UPDATE,YYYY-MM-DD'
  printf, lun, 'intensity         F10.2        SURFACE,BRIGHTNESS,KRAYLEIGHS'
  printf, lun, 'err_intensity     F10.2        ERROR,INTENSITY,KRAYLEIGHS'
  printf, lun, 'lat_subsolar      F8.2         SUBSOLAR,LATITUDE,DEG'
  printf, lun, 'long_subsolar     F9.2         SUBSOLAR,LONGITUDE,DEG'
  printf, lun, 'ra                F11.5        RIGHT-J2000,ASCENSION,DEGREES'
  printf, lun, 'dec               F11.5        DECLINATION,(J2000),DEGREES'
  printf, lun, ''

  printf, lun, '#index'
  printf, lun, 'nday              sort/index'
  printf, lun, 'obj_code          sort/index'
  printf, lun, 'line_code         sort/index'
  printf, lun, 'phi               sort/index'
  printf, lun, 'long_3            sort/index'
  printf, lun, 'lat_sub           sort/index'
  printf, lun, 'zd                sort/index'
  printf, lun, 'dice              sort/index'
  printf, lun, 'io_lat            sort/index'
  printf, lun, 'nrows             sort/index'
  printf, lun, 'ra                sort/index'
  printf, lun, 'dec               sort/index'
  printf, lun, 'line              index'
             
  ;; Unfortunately, one pointer cannot point to more than one
  ;; database.  If I really need to reference back, I'll have to
  ;; fiddle
  printf, lun, ' '
  printf, lun, '#pointers'
  printf, lun, 'nday              ', files[1]

  close, lun
  free_lun, lun


  oldpriv=!priv
  !priv=2
  for i=0,nf-1 do begin
     message, 'Initializing '+files[i]+'  database', /CONTINUE
     ;; The /EXTERNAL is important for binary compatibility between
     ;; the Suns and PCs
     dbcreate, files[i], newindex, newdb, /EXTERNAL
  endfor
  !priv=oldpriv

end
