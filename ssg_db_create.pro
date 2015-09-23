;+
; $Id: ssg_db_create.pro,v 1.10 2015/03/04 15:52:32 jpmorgen Exp $

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
  
  nf=4
  files=strarr(nf)
  files[0] = 'ssg_reduce'
  files[1] = 'oi_6300_fit'
  files[2] = 'oi_6300_fit_ext'
  files[3] = 'io_oi_analyze'
  for i=0,nf-1 do begin
     tmp=file_search(string(files[i],'.dbd'), COUNT=count)
     if count gt 0 and NOT keyword_set(newdbd) then $
       message, 'ERROR: will not overwrite '+ tmp+' DataBase Descriptor file unless /ERASEDBD is specified.  If you are just making changes to the formats, it is OK to /ERASEDBD.  The software you are using to create the DBD file should be stored in a revision control system so that you can track changes and get back  to a version that works with a particular binary database (e.g. everything has to line up in the database).'
     tmp=file_search(string(files[i],'.dbx'), COUNT=count)
     if count eq 0 then newindex=1 else begin
        if NOT keyword_set(newindex) then begin
           newindex = 0
           message, /CONTINUE, 'NOTE: Preserving index file ' +tmp
        endif
     endelse

     tmp=file_search(string(files[i],'.dbf'), COUNT=count)
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
  printf, lun, '100000'
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
  printf, lun, 'obj_code        B*1		object code (Jup=0,Io=1,Europa=2,Ganymede=3,Callst=4,Night Sky=5,Dark=6,comp=7,lamp=8,daysky=9,other=10, Unknown=255)'
  ;; I'd like to make these B*2, but dbext translates that into a 1
  ;; byte variable.
  printf, lun, 'bad		I*2		bitmap'
;; 16384=unspecified (e.g. bad slicer shape or diseprsion for the whole night)
;; 8192=overclock
;; 4096=slicer position
;; 2048=camera rotation
;; 1024=wavelen scale gap
;; 512=spectral extraction
;; ???=slicer shape
;; 128=IP
;; 64=extraction, maybe these should be negative....=solar fitting, 32=narrow atm fitting, 16=broad atm fitting, 8=airglow overlap, 4=Io [OI] velocity wrong, 2=width wrong, 1=large error bar, 0=good'
  printf, lun, 'no_fit		I*2		bitmap flag for polynomial fitting.'
;; Bits indicate which to exclude
;; 1 = slicer bottom
;; 2 = slicer top
;; 3 = camera rotation

  printf, lun, 'gain		R*4		derived gain, electrons per adu'
  printf, lun, 'rdnoise		R*4		derived read noise, electrons'
  printf, lun, 'n_ovrclk	I*2		number of overclock rows'
  printf, lun, 'med_ovrclk(800) R*4		median overclock spectrum'
  printf, lun, 'av_ovrclk(800)	R*4		average overclock spectrum, CR removed'
  printf, lun, 'med_bias	R*4		median bias value from overclock region'
  printf, lun, 'av_bias		R*4		average bias value from overclock region'
  printf, lun, 'stdev_bias	R*4		stdev bias value from overclock region'
  printf, lun, 'med_back	R*4		median background light value measured from area outside slicer pattern after bias subtraction'
  printf, lun, 'av_back		R*4		average background light value (see med_back)'
  printf, lun, 'stdev_back	R*4		stdev background light value (see med_back)'
  printf, lun, 'bias_fname	C*80		bias image associated with this file'
  printf, lun, 'dark_fname	C*80		dark image associated with this file'
  printf, lun, 'lampflat_dir	C*80		directory containing reduced lamp flats'
  printf, lun, 'skyflat_dir	C*80		directory containing reduced sky flats'
  printf, lun, 'm_sli_bot	R*4		Measured slicer bottom edge (pix from bottom of image)'
  printf, lun, 'e_sli_bot	R*4		Measured slicer bottom edge error'
  printf, lun, 'sli_bot		R*4		Slicer bottom edge to use'
  printf, lun, 'm_sli_top	R*4		Measured slicer top edge (pix from bottom of image)'
  printf, lun, 'e_sli_top	R*4		Measured slicer top edge error'
  printf, lun, 'sli_top		R*4		Slicer top edge to use'
  printf, lun, 'sli_cent	R*4		Best slicer center'
  printf, lun, 'e_sli_cent	R*4		Slicer center error'
  printf, lun, 'm_cam_rot	R*4		Measured camera rotation: clockwise deviation of flatfield pattern on CCD.'
  printf, lun, 'e_cam_rot	R*4		Measured camera rotation: clockwise deviation of flatfield pattern on CCD.'
  printf, lun, 'cam_rot		R*4		Predicted camera rotation based on fit'
  printf, lun, 'edge_coef(4)	R*4		coefficients describing the background light at the edge of each image'
  printf, lun, 'edge_coef_err(4) R*4		1-sigma errors in edge_coef'
  printf, lun, 'm_slice(100)	R*4		10x10 array of polynomial coefsexpressing measured spectral variation of slicer shape.' ;  First row is a 10th order polynomial description of a slice at the midpoint of the spectrum.  Columns are polynomials in dispersion pixel number describing the spectral change in the slicer shape.'
  printf, lun, 'slice(100)	R*4		Predicted slicer shape for this image'
  printf, lun, 'flat_cut	R*4		flatfield value below which all pixels are set to NAN'
  printf, lun, 'sky_cut		R*4		skyflat value below which all pixels are set to NAN'
  printf, lun, 'cr_cut		R*4		Using a template image, sigma value above which pixel is assumed to be a cosmic ray hit'
  printf, lun, 'ncr		R*4		estimated number of cosmic ray hits inside flatcut'
  printf, lun, 'nbad		I*2		number of bad pixels (from CR and bad cols) inside flatcut'
  printf, lun, 'nbad_col	I*2		number of columns with strange Xdisp spectra (see ssg_cr_replace)'
  printf, lun, 'm_IP(100)	R*4		10x10 array describing instrument profile.  Like slice shape, reference is midpoint'
  printf, lun, 'IP(100)		R*4		Predicted IP for this spectrum'
  printf, lun, 'm_dispers(10)	R*4		up to 10 polynomial coefficients describing measured dispersion relation.  Ref to  midpoint '
  printf, lun, 'dispers(10)	R*4		individually fit dispersion (starts with time varying fit to m_dispers'
  printf, lun, 'disp_pix(800)	R*4		indecies of columns in reduced image used to make wavelen and spectrum'
  printf, lun, 'xdisp_pix(400)	R*4		indecies of rows in reduced image used to make xdisp_spec'
  printf, lun, 'wavelen(800)	R*4		wavelength axis (angstroms), no offset'
  printf, lun, 'spectrum(800)	R*4		reduced spectrum with bad pixels omitted'
  printf, lun, 'spec_err(800)	R*4		error estimate of spectrum'
  printf, lun, 'cross_disp(400)	R*4		full reduced cross-dispersion spectrum'
  printf, lun, 'cross_err(400)	R*4		statistical error on cross dispersion spectrum'
  printf, lun, 'med_spec	R*4		median spectral value'
  printf, lun, 'av_spec		R*4		average spectral value'
  printf, lun, 'min_spec	R*4		minimum spectral value'
  printf, lun, 'max_spec	R*4		maximum spectral value'
  printf, lun, 'med_cross	R*4		median cross dispersion value'
  printf, lun, 'av_cross	R*4		average cross dispersion value'
  printf, lun, 'min_cross	R*4		minimum cross dispersion value'
  printf, lun, 'max_cross	R*4		maximum cross dispersion value'
  printf, lun, 'cross_dcog	R*4		distance in pix of xdisp center of gravity from slicer center'
  printf, lun, 'n_disp		I*2             number of elements in dispersion spectrum'
  printf, lun, 'n_xdisp		I*2             number of elements in cross-dispersion spectrum'

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
  printf, lun, 'typecode	I4		0b1d,2c3f,4s5o'
  printf, lun, 'bad		I4		0,is,good'
  printf, lun, 'no_fit		I4		poly,fit,flag'
  printf, lun, 'gain		F7.3		calc,gain,e_/DN'
  printf, lun, 'rdnoise		F7.3		calc,rdnoise,e_'
  printf, lun, 'n_ovrclk	I4		novr,clck,rows'
  printf, lun, 'med_ovrclk	F7.3		median,overclk,spec'
  printf, lun, 'av_ovrclk	F7.3		avovrc,spec,no_CR'
  printf, lun, 'med_bias	F7.3		median,ovrclk,value'
  printf, lun, 'av_bias		F7.3		averag,ovrclk,value'
  printf, lun, 'stdev_bias	F7.3		stdev,ovrclk,value'
  printf, lun, 'med_bias	F7.3		median,backgd,value'
  printf, lun, 'av_bias		F7.3		averag,backgd,value'
  printf, lun, 'stdev_bias	F7.3		stdev,backgd,value'
  printf, lun, 'bias_fname	A80		bias,reference,file'
  printf, lun, 'dark_fname	A80		dark,reference,file'
  printf, lun, 'lampflat_flat	A80		flatfield,reference,dir'
  printf, lun, 'skyflat_flat	A80		skyflatreference,dir'
  printf, lun, 'm_sli_bot	F7.3		measred,slicer,bottom'
  printf, lun, 'e_sli_bot	F7.3		slicer,bottom,error'
  printf, lun, 'sli_bot		F7.3		to_use,slicer,bottom'
  printf, lun, 'm_sli_top	F7.3		measred,slicer,top'
  printf, lun, 'e_sli_top	F7.3		slicer,top,error'
  printf, lun, 'sli_top		F7.3		to_use,slicer,top'
  printf, lun, 'sli_cent	F7.3		best,slicer,center'
  printf, lun, 'e_sli_cent	F7.3		slicer,center,error'
  printf, lun, 'm_cam_rot	F7.3		measred,cam_rot,clockws'
  printf, lun, 'e_cam_rot	F7.3		measred,cam_rot,error'
  printf, lun, 'cam_rot		F7.3		to_use,cam_rot,clockws'
  printf, lun, 'm_slice		F7.3		measred,slicer,shape'
  printf, lun, 'slice		F7.3		pred,slicer,shape'
  printf, lun, 'flat_cut	F7.3		flat,cut,4trim'
  printf, lun, 'sky_cut		F7.3		sky,cut,4trim'
  printf, lun, 'cr_cut		F7.3		cosmic,ray,cutval'
  printf, lun, 'ncr		F7.3		est,number,CR'
  printf, lun, 'nbad		F7.3		num,bad,pix'
  printf, lun, 'nbad_col	F7.3		num,strange,cols'
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
  printf, lun, 'cross_dcog	F7.3		delta,sli_cen,sli_COG'
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
;  printf, lun, 'flat_cut	sort/index'
;  printf, lun, 'cr_cut		sort/index'
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

;  ;; Unfortunately, one pointer cannot point to more than one
;  ;; database.  If I really need to reference back, I'll have to
;  ;; fiddle
;  printf, lun, ' '
;  printf, lun, '#pointers'
;  printf, lun, 'nday              ', files[1]

  close, lun
  free_lun, lun

  ;; FITTING DATABASE
  openw, lun, string(files[1], '.dbd'), /get_lun
  printf, lun, '#title'
  printf, lun, 'Io [OI] Spectral Fitting Database'
  printf, lun, ' '
  printf, lun, '#maxentries'
  printf, lun, '100000'
  printf, lun, ' '
  printf, lun, '#items'
  printf, lun, 'nday		R*8		decimal day since 1990-Jan-01 (midpoint)'
  printf, lun, 'new_spec	I*2		Flag indicating a new spectrum has been generated since last fit'
  printf, lun, 'bad		I*2		bitmap'
  printf, lun, 'fit_vers	B*1		fit version'
  printf, lun, 'object          C*13         object name'
  printf, lun, 'obj_code        B*1		object code (Jup=0,Io=1,Europa=2,Ganymede=3,Callst=4,Sky=5,Other=6, Unknown=255)'
  printf, lun, 'm_dispers(10)	R*4		up to 10 polynomial coefficients describing measured dispersion relation.  Ref to  midpoint '
  printf, lun, 'dispers(10)	R*4		individually fit dispersion (starts with time varying fit to m_dispers'
  printf, lun, 'm_IP(100)	R*4		10x10 array describing instrument profile.  Like slice shape, reference is midpoint'
  printf, lun, 'IP(100)		R*4		Predicted IP for this spectrum'
  ;; Duplicate these, since cross-referencing doesn't work that well
  printf, lun, 'wavelen(800)	R*4		wavelength axis (angstroms), no offset'
  printf, lun, 'spectrum(800)	R*4		reduced spectrum with bad pixels omitted'
  printf, lun, 'spec_err(800)	R*4		error estimate of spectrum'
  printf, lun, 'cross_disp(400)	R*4		full reduced cross-dispersion spectrum'
  printf, lun, 'cross_err(400)	R*4		statistical error on cross dispersion spectrum'
  printf, lun, 'med_spec	R*4		median spectral value'
  printf, lun, 'av_spec		R*4		average spectral value'
  printf, lun, 'min_spec	R*4		minimum spectral value'
  printf, lun, 'max_spec	R*4		maximum spectral value'
  printf, lun, 'med_cross	R*4		median cross dispersion value'
  printf, lun, 'av_cross	R*4		average cross dispersion value'
  printf, lun, 'min_cross	R*4		minimum cross dispersion value'
  printf, lun, 'max_cross	R*4		maximum cross dispersion value'
  printf, lun, 'n_disp		I*2             number of elements in dispersion spectrum'
  printf, lun, 'n_xdisp		I*2             number of elements in cross-dispersion spectrum'
  printf, lun, 'cross_dcog	R*4		distance in pix of xdisp center of gravity from slicer center'
  printf, lun, 'fit_date	C*10		database update date (yyyy-mm-dd)'
  printf, lun, 'nfree		I*2		number of free parameters'
  printf, lun, 'chisq		R*8             chi^2'
  printf, lun, 'redchisq	R*8             reduced chi^2'
  printf, lun, 'value(200)	R*8             value       '
  printf, lun, 'perror(200)	R*8             value error bar'
  printf, lun, 'fixed(200)	R*8             fixed       '
  printf, lun, 'llimited(200)	I*2             limited     '    
  printf, lun, 'rlimited(200)	I*2             limited     '    
  printf, lun, 'llimits(200)	R*8             limits      '    
  printf, lun, 'rlimits(200)	R*8             limits      '    
  printf, lun, 'vfID(200)	I*2             vfID        '    
  printf, lun, 'ssgID(200)	I*2             ssgID       '    
  printf, lun, 'ssggroupID(200)	I*2             ssggroupID  '    
  printf, lun, 'ssgrwl(200)	R*8             ssgrwl      '    
  printf, lun, 'ssgowl(200)	R*8             ssgowl      '    
  printf, lun, 'ssgdop(200)	I*2		ssgdop      '    
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
  printf, lun, 'redchisq	sort/index'
  printf, lun, 'fit_vers	sort/index'

  ;; Unfortunately, one pointer cannot point to more than one
  ;; database.  Furthermore, the databases have to have the same
  ;; entries.  For the text extension if the fitting, this works
  ;; fine.  For getting back to the reduced, I have to fiddle.

  printf, lun, ' '
  printf, lun, '#pointers'
  printf, lun, 'nday              ', files[2]

  close, lun
  free_lun, lun

  ;; FITTING extension DATABASE (mostly text but also any new features)
  openw, lun, string(files[2], '.dbd'), /get_lun
  printf, lun, '#title'
  printf, lun, 'Io [OI] Spectral Fitting Database (extension)'
  printf, lun, ' '
  printf, lun, '#maxentries'
  printf, lun, '100000'
  printf, lun, ' '
  printf, lun, '#items'
  printf, lun, 'nday		R*8		decimal day since 1990-Jan-01 (midpoint)'
  ;; dbbuild does not like arrays of strings.  Modified dbbuild myself
  ;; in ../misc
  printf, lun, 'parname(200)	C*20		parname     '    
  printf, lun, 'tied(200)	C*80		tied     '    
;  printf, lun, 'weighting(800)	R*4		this spectrum multiplies the error bars used for the fit so points far from the line of interest are weighted less'
;  printf, lun, 'cen_weight	R*4		center pixel of weighting function, 0=not a simple V-shaped function'
;  printf, lun, 'med_weight	R*4		median of weighting function'
;  printf, lun, 'av_weight	R*4		average of weighting function'
;  printf, lun, 'min_weight	R*4		min of weighting function'
;  printf, lun, 'max_weight	R*4		max of weighting function'
;  printf, lun, 'weight_vers	B*1		weighting function version'


  printf, lun, ' '
  printf, lun, '#formats'
  printf, lun, 'nday		F11.5		DECIMAL,DAY,>90/01/01'

  printf, lun, ' '
  printf, lun, '#index'
  printf, lun, 'nday		sort/index'
;  printf, lun, 'cen_weight	sort/index'
;  printf, lun, 'med_weight	sort/index'
;  printf, lun, 'av_weight	sort/index'
;  printf, lun, 'min_weight	sort/index'
;  printf, lun, 'max_weight	sort/index'
;  printf, lun, 'weight_vers	sort/index'
;  printf, lun, 'fit_vers	sort/index'

  ;; Unfortunately, one pointer cannot point to more than one
  ;; database.  If I really need to reference back, I'll have to
  ;; fiddle
  printf, lun, ' '
  printf, lun, '#pointers'
  printf, lun, 'nday              ', files[1]

  close, lun
  free_lun, lun

  ;; ANALYSIS DATABASE
  ;; This started as Melanie's database and was modified a
  ;; little to make ssg_ana_init easier
  openw, lun, string(files[3], '.dbd'), /get_lun
  printf, lun, '#title'
  printf, lun, 'Io [OI] Analysis Database'
  printf, lun, ' '
  printf, lun, '#maxentries'
  printf, lun, '100000'

  printf, lun, '#items'
  printf, lun, 'date              C*10         yyyy-mm-dd'
  printf, lun, 'time              C*8          hh:mm:ss (start:UT)'
  printf, lun, 'nday              R*8          decimal day since 1990-Jan-01 (midpoint)'
  printf, lun, 'object            C*13         object name'
  printf, lun, 'obj_code	  B*1		object code (Jup=0,Io=1,Europa=2,Ganymede=3,Callst=4,Sky=5,Other=6, Unknown=255)'
  printf, lun, 'line              C*7          line of observation ([O I],Na D,H-alpha(6563A))   '
  printf, lun, 'lambda            I*2          wavelength (Angstroms) (e.g.,[O I] 6300, Na D 5893)'
  printf, lun, 'irafname          C*16         name of original iraf file'
  printf, lun, 'specname          C*25         name of extracted spectrum file'
  printf, lun, 'exptime           R*4          exposure time (seconds)'
  printf, lun, 'nrows             I*2          number of rows extracted'
  printf, lun, 'npix              I*2          number of pixels extracted'
  printf, lun, 'p_date            C*10         processing date (yyyy-mm-dd)'
  ;; Down to here is entered in one fell swoop in ssg_ana_init
  ;; This next section more-or-less matches the HORIZONS column names
  ;; except common abreviations
  printf, lun, 'ra                R*4          astrometric right ascension J2000 (degrees)'
  printf, lun, 'dec               R*4          astrometric declination J2000 (degrees)'
  printf, lun, 'zd                R*4          zenith distance (midpoint:degrees)' ;; HORZIONS likes el
  printf, lun, 'ha                R*4          hour angle (midpoint:decimal)'
  printf, lun, 'am                R*4          air mass (midpoint)'
  printf, lun, 'ang_dia           R*4          Io(moon) diameter (arcsec)'
  printf, lun, 'ob_lon		  R*4          sub-longitude of Io(moon) center from the observer (degrees)'
  printf, lun, 'ob_lat		  R*4          sub-latitude of Io(moon) center from the Sun (degrees)'
  printf, lun, 'sl_lon            R*4          sub-longitude of Io(moon) center from the Sun (degrees)'
  printf, lun, 'sl_lat            R*4          sub-latitude of Io(moon) center from the observer (degrees)'
  printf, lun, 'spa               R*4          solar phase angle (S-T-O) (midpoint:degrees)'
  printf, lun, 'r                 R*8          heliocentric distance (AU)'
  printf, lun, 'rdot              R*4          heliocentric velocity (midpoint:km/s)'
  printf, lun, 'delta             R*8          geocentric distance (AU)'
  printf, lun, 'deldot            R*4          geocentric velocity (midpoint:km/s)'
  printf, lun, '1lt	          R*8          1-way light travel time (s)'
  ;; Get accurate light times for each object
  printf, lun, 'Jup_1lt	          R*8          Jupiter 1-way light travel time (s)'
  printf, lun, 'Io_1lt	          R*8          Io 1-way light travel time (s)'
  printf, lun, 'Eur_1lt	          R*8          Europa 1-way light travel time (s)'
  printf, lun, 'Gan_1lt	          R*8          Ganymede 1-way light travel time (s)'
  printf, lun, 'Cal_1lt	          R*8          Callisto 1-way light travel time (s)'
  
  ;; Parameters of interest for all Galilean satellites are calculated
  ;; and stored here to check for correlations.  The first set are for
  ;; whatever moon was actually observed.
  printf, lun, 'long_3            R*4          moon sys III longitude (midpoint:degrees)'
  printf, lun, 'lat_eq		  R*4          moon equatorial latitude (midpoint:degrees)'
  printf, lun, 'lat_B	          R*4          moon magnetic latitude (midpoint:degrees)'
  printf, lun, 'd_CE              R*4          distance of moon to magnetic centrifugal equator (midpt:Jovian Radii)'
  printf, lun, 'b_field           R*4          moon magnetic field strength (nT)'
  printf, lun, 'lshell            R*4          magnetic L-shell at moon (Jovian Radii)(offset tilted dipole)'
  printf, lun, 'lat_tan_B         R*4          moon-centric latitude of B-field tangent point (degrees)'

  printf, lun, 'Io_long_3         R*4          Io sys III longitude (midpoint:degrees)'
  printf, lun, 'Io_lat_eq	  R*4          Io equatorial latitude (midpoint:degrees)'
  printf, lun, 'Io_lat_B          R*4          Io magnetic latitude (midpoint:degrees)'
  printf, lun, 'Io_d_CE           R*4          distance of Io to magnetic centrifugal equator (midpt:Jovian Radii)'
  printf, lun, 'Io_b_field        R*4          Io magnetic field strength (nT)'
  printf, lun, 'Io_lshell         R*4          magnetic L-shell at Io (Jovian Radii)(offset tilted dipole)'
  printf, lun, 'Io_lat_tan_B      R*4          Io-centric latitude of B-field tangent point (degrees)'

  printf, lun, 'Eur_long_3        R*4          Europa sys III longitude (midpoint:degrees)'
  printf, lun, 'Eur_lat_eq	  R*4          Europa equatorial latitude (midpoint:degrees)'
  printf, lun, 'Eur_lat_B         R*4          Europa magnetic latitude (midpoint:degrees)'
  printf, lun, 'Eur_d_CE          R*4          distance of Europa to magnetic centrifugal equator (midpt:Jovian Radii)'
  printf, lun, 'Eur_b_field       R*4          Europa magnetic field strength (nT)'
  printf, lun, 'Eur_lshell        R*4          magnetic L-shell at Europa (Jovian Radii)(offset tilted dipole)'
  printf, lun, 'Eur_lat_tan_B     R*4          Europa-centric latitude of B-field tangent point (degrees)'

  printf, lun, 'Gan_long_3        R*4          Ganymede sys III longitude (midpoint:degrees)'
  printf, lun, 'Gan_lat_eq	  R*4          Ganymede equatorial latitude (midpoint:degrees)'
  printf, lun, 'Gan_lat_B         R*4          Ganymede magnetic latitude (midpoint:degrees)'
  printf, lun, 'Gan_d_CE          R*4          distance of Ganymede to magnetic centrifugal equator (midpt:Jovian Radii)'
  printf, lun, 'Gan_b_field       R*4          Ganymede magnetic field strength (nT)'
  printf, lun, 'Gan_lshell        R*4          magnetic L-shell at Ganymede (Jovian Radii)(offset tilted dipole)'
  printf, lun, 'Gan_lat_tan_B     R*4          Ganymede-centric latitude of B-field tangent point (degrees)'

  printf, lun, 'Cal_long_3        R*4          Callisto sys III longitude (midpoint:degrees)'
  printf, lun, 'Cal_lat_eq	  R*4          Callisto equatorial latitude (midpoint:degrees)'
  printf, lun, 'Cal_lat_B         R*4          Callisto magnetic latitude (midpoint:degrees)'
  printf, lun, 'Cal_d_CE          R*4          distance of Callisto to magnetic centrifugal equator (midpt:Jovian Radii)'
  printf, lun, 'Cal_b_field       R*4          Callisto magnetic field strength (nT)'
  printf, lun, 'Cal_lshell        R*4          magnetic L-shell at Callisto (Jovian Radii)(offset tilted dipole)'
  printf, lun, 'Cal_lat_tan_B     R*4          Callisto-centric latitude of B-field tangent point (degrees)'

  printf, lun, 'phi               R*4          moon orbital phase (midpoint:degrees)'
  printf, lun, 'side              C*4          moon side of Jupiter (observed) east/west'

  printf, lun, 'Io_phi            R*4          Io orbital phase (midpoint:degrees)'
  printf, lun, 'Eur_phi           R*4          Europa orbital phase (midpoint:degrees)'
  printf, lun, 'Gan_phi           R*4          Ganymede orbital phase (midpoint:degrees)'
  printf, lun, 'Cal_phi           R*4          Callisto orbital phase (midpoint:degrees)'


;  printf, lun, 'jnp               R*4          Jovian north pole position angle (deg)'

  printf, lun, 'deldot_m          R*4          measured geocentric velocity (km/s)'
  printf, lun, 'err_deldot_m      R*4          error in measured geocentric velocity (km/s)'
  printf, lun, 'fline             R*4          measured line flux (electrons/sec)'
  printf, lun, 'err_fline         R*4          error in line flux (electrons/sec)'
  printf, lun, 'fcont             R*4          measured continuum flux (electrons/sec/Angstrom)'
  printf, lun, 'err_fcont         R*4          error in continuum flux (electrons/sec/Angstrom)'
  printf, lun, 'wc                R*4          measured convolved FWHM (milli Angstroms)'
  printf, lun, 'err_wc            R*4          error in convolved FWHM (milli Angstroms)'
  printf, lun, 'wd                R*4          deconvolved FWHM (milli Angstroms)'
  printf, lun, 'err_wd            R*4          error in deconvolved FWHM (milli Angstroms)'
  printf, lun, 'weq               R*4          equivalent width (milli Angstroms)'
  printf, lun, 'err_weq           R*4          error in equivalent width (milli Angstroms)'
  printf, lun, 'alf               R*4          apparent line flux (photons/sec/cm^2)'
  printf, lun, 'err_alf           R*4          error in apparent line flux (photons/sec/cm^2)'
  printf, lun, 'ag_ew             R*4          airglow equivalent width (milli Angstroms)'
  printf, lun, 'err_ag_ew         R*4          error airglow equivalent width (milli Angstroms)'
  printf, lun, 'ag_wc             R*4          airglow line convolved width (milli Angstroms)'
  printf, lun, 'err_ag_wc         R*4          error in airglow line convolved width (milli Angstroms)'
  printf, lun, 'ag_flux           R*4          airglow line flux (electrons/sec)'
  printf, lun, 'err_ag_flux       R*4          error in airglow line flux (electrons/sec)'
  printf, lun, 'redchisq          R*4          reduced chi-squared        '
  printf, lun, 'freeparam         I*2          number of free parameters in the model'
  printf, lun, 'numlines          I*2          number of spectral lines used in the model'
  printf, lun, 'rows(150)         L*1          rows extracted (0=false, 1=true)'
  printf, lun, 'quality           C*7          quality assessment flag'
  printf, lun, 'ip                R*4          instrumental profile (FWHM: milli-Angstroms)'
  printf, lun, 'disp              R*4          dispersion (milli-Angstroms/pixel)'
  printf, lun, 'refpix            R*4          nominal reference pixel'
  printf, lun, 'refwave           R*4          nominal reference wavelength (Angstroms)'
  printf, lun, 'comp_min          R*4          comp lamp FWHM minimum (Angstroms)'
  printf, lun, 'comp_max          R*4          comp lamp FWHM maximum (Angstroms)'
  printf, lun, 'vers              C*4          model version'
  printf, lun, 'db_date           C*10         database update date (yyyy-mm-dd)'
  printf, lun, 'intensity         R*4          surface brightness (alf/area of disk of Io) (kRaleighs)'
  printf, lun, 'err_intensity     R*4          error in surface brightness (kRaleighs)'
  printf, lun, 'nn_DOWL           R*4          Nearest neighbor Delta Observed Wavelength (Angstroms)'
  printf, lun, 'nn_ew             R*4          Nearest neighbor equivalent width'
  printf, lun, 'nn_Dw             R*4          Nearest neighbor Dopper width'
  printf, lun, 'nn_Lw             R*4          Nearest neighbor Lorentzian width'
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
  printf, lun, 'exptime           I4           EXPO,TIME,SEC'
  printf, lun, 'nrows	          I4           NUM,ROWS,PIX'
  printf, lun, 'p_date            A11          PROCESSING,DATE,YYYY-MM-DD'

  printf, lun, 'ra                F11.5        RIGHT-J2000,ASCENSION,DEGREES'
  printf, lun, 'dec               F11.5        DECLINATION,(J2000),DEGREES'
  printf, lun, 'zd                F6.3         ZENITH,DIST,DEG'
  printf, lun, 'ha                F6.3         HOUR,ANGLE'
  printf, lun, 'am                F7.3          ,AIRMASS'
  printf, lun, 'ang_dia           F6.3         ANG,DIA,ARCSEC'
  printf, lun, 'ob_long           F9.2         SUB-LONG,OBS,DEG'
  printf, lun, 'ob_lat            F9.2         SUB-LAT,OBS,DEG'
  printf, lun, 'sl_lon            F9.2         SUB-LON,SUN,DEG'
  printf, lun, 'sl_lat            F9.2         SUB-LAT,SUN,DEG'
  printf, lun, 'spa               F10.2        SOLAR,PHASE,ANGLE(DEG)'
  printf, lun, 'r                 F10.8        HELIO-,CENTRIC,DIST(AU)'
  printf, lun, 'rdot              F10.4        HELIO-,CENTRIC,VEL(KM/S)'
  printf, lun, 'delta             F10.8        GEOCENTRIC,DISTANCE,(AU) '
  printf, lun, 'deldot            F10.4        GEOCENTRIC,VELOCITY,(KM/S)'
  printf, lun, '1lt               F10.4        OBJ,1-WAY-LT,S'
  printf, lun, 'Jup_1lt           F10.4        JUP,1-WAY-LT,S'
  printf, lun, 'Io_1lt            F10.4        IO,1-WAY-LT,S'
  printf, lun, 'Eur_1lt           F10.4        EUR,1-WAY-LT,S'
  printf, lun, 'Gan_1lt           F10.4        GAN,1-WAY-LT,S'
  printf, lun, 'Cal_1lt           F10.4        CAL,1-WAY-LT,S'

  printf, lun, 'long_3            F9.2         LONGITUDE,III,DEG'
  printf, lun, 'lat_eq            F8.2         EQ,LATITUDE,DEG'
  printf, lun, 'lat_B             F8.2         MAG,LATITUDE,DEG'
  printf, lun, 'd_CE              F8.3         DIST.,CENTRIP-EQ,Rj'
  printf, lun, 'b_field           F8.2         FIELD,STRENGTH,nT'
  printf, lun, 'lshell            F7.2         MAG,L-SHELL,Rj'
  printf, lun, 'lat_tan_B         F8.2         MOON,MAG-TAN-LAT,,DEG'

  printf, lun, 'Io_long_3         F9.2         IO_LONGITUDE,III,DEG'
  printf, lun, 'Io_lat_eq         F8.2         IO_EQ,LATITUDE,DEG'
  printf, lun, 'Io_lat_B          F8.2         IO_MAG,LATITUDE,DEG'
  printf, lun, 'Io_d_CE           F8.3         IO_DIST.,CENTRIP-EQ,Rj'
  printf, lun, 'Io_b_field        F8.2         IO_FIELD,STRENGTH,nT'
  printf, lun, 'Io_lshell         F7.2         IO_MAG,L-SHELL,Rj'
  printf, lun, 'Io_lat_tan_B      F8.2         IO,MAG-TAN-LAT,,DEG'

  printf, lun, 'Eur_long_3        F9.2         EUR_LONGITUDE,III,DEG'
  printf, lun, 'Eur_lat_eq        F8.2         EUR_EQ,LATITUDE,DEG'
  printf, lun, 'Eur_lat_B         F8.2         EUR_MAG,LATITUDE,DEG'
  printf, lun, 'Eur_d_CE          F8.3         EUR_DIST.,CENTRIP-EQ,Rj'
  printf, lun, 'Eur_b_field       F8.2         EUR_FIELD,STRENGTH,nT'
  printf, lun, 'Eur_lshell        F7.2         EUR_MAG,L-SHELL,Rj'
  printf, lun, 'Eur_lat_tan_B     F8.2         EUR,MAG-TAN-LAT,,DEG'

  printf, lun, 'Gan_long_3        F9.2         GAN_LONGITUDE,III,DEG'
  printf, lun, 'Gan_lat_eq        F8.2         GAN_EQ,LATITUDE,DEG'
  printf, lun, 'Gan_lat_B         F8.2         GAN_MAG,LATITUDE,DEG'
  printf, lun, 'Gan_d_CE          F8.3         GAN_DIST.,CENTRIP-EQ,Rj'
  printf, lun, 'Gan_b_field       F8.2         GAN_FIELD,STRENGTH,nT'
  printf, lun, 'Gan_lshell        F7.2         GAN_MAG,L-SHELL,Rj'
  printf, lun, 'Gan_lat_tan_B     F8.2         GAN,MAG-TAN-LAT,,DEG'

  printf, lun, 'Cal_long_3        F9.2         CAL_LONGITUDE,III,DEG'
  printf, lun, 'Cal_lat_eq        F8.2         CAL_EQ,LATITUDE,DEG'
  printf, lun, 'Cal_lat_B         F8.2         CAL_MAG,LATITUDE,DEG'
  printf, lun, 'Cal_d_CE          F8.3         CAL_DIST.,CENTRIP-EQ,Rj'
  printf, lun, 'Cal_b_field       F8.2         CAL_FIELD,STRENGTH,nT'
  printf, lun, 'Cal_lshell        F7.2         CAL_MAG,L-SHELL,Rj'
  printf, lun, 'Cal_lat_tan_B     F8.2         CAL,MAG-TAN-LAT,,DEG'

  printf, lun, 'phi               F8.2         ORBITAL,PHASE,DEG  '
  printf, lun, 'side              A5           EAST/,WEST'

;  printf, lun, 'jnp               F7.2         JOVIAN,N_POLE,POS_ANG'

  printf, lun, 'deldot_m          F10.4        MEASURED,GEOCENTRIC,VEL(KM/S)'
  printf, lun, 'err_deldot_m      F8.4         ERROR,DELDOT_M,KM/S'
  printf, lun, 'fline             F7.4         LINE,FLUX,e/s       '
  printf, lun, 'err_fline         F7.4         ERROR,FLINE,e/s '
  printf, lun, 'fcont             F9.4         CONTINUUM,FLUX,e/s/A'
  printf, lun, 'err_fcont         F9.4         ERROR,FCONT,e/s/A'
  printf, lun, 'wc                F8.6         FWHM,CONV,mA'
  printf, lun, 'err_wc            F8.6         ERROR,WC,mA'
  printf, lun, 'wd                F8.6         FWHM,DECONV,mA'
  printf, lun, 'err_wd            F8.6         ERROR,WD,mA'
  printf, lun, 'weq               F8.6         EQUIVAL,WIDTH,mA'
  printf, lun, 'err_weq           F8.6         ERROR,WEQ,A'
  printf, lun, 'alf               F9.6         ABSOLUTE,LINE,FLUX'
  printf, lun, 'err_alf           F9.6         ERROR,ALF'
  printf, lun, 'ag_ew             F8.5         AIRGLOW,EW,mA'
  printf, lun, 'err_ag_ew         F8.5         ERROR,AIRGLOW,EW'
  printf, lun, 'ag_wc             F8.5         AIRGLOW,WIDTH,mA'
  printf, lun, 'err_ag_wc	  F8.5         ERROR,AIRGLOW,WIDTH'
  printf, lun, 'ag_flux           F8.5         AIRGLOW,FLUX,e/s'
  printf, lun, 'err_ag_flux       F8.5         ERROR,AIRGLOW,FLUX'
  printf, lun, 'redchisq          F7.4         REDUCED,CHI^2'
  printf, lun, 'freeparam         I5           NUM,FREE,PARAM'
  printf, lun, 'numlines          I5           NUM,LINES'
  printf, lun, 'rows              I7           ROWS,0=FALSE,1=TRUE'
;  printf, lun, 'n_disp  	  I*2          number of elements in dispersion spectrum'
;  printf, lun, 'n_xdisp		  I*2          number of elements in cross-dispersion spectrum'
  printf, lun, 'quality           A7            ,QUALITY'
  printf, lun, 'ip                F7.4         INSTR.,PROFILE,mA'
  printf, lun, 'disp              F8.6         DISPERSN'
  printf, lun, 'refpix            F9.3         REFERENCE,PIXEL'
  printf, lun, 'refwave           F9.3         REFERENCE,WAVE-,LENGTH(A)'
  printf, lun, 'comp_min          F9.2         COMP_LAMP,FWHM,MIN(A)'
  printf, lun, 'comp_max          F9.2         COMP_LAMP,FWHM,MAX(A)'
  printf, lun, 'vers              A7           MODEL,VERSION'
  printf, lun, 'db_date           A11          DATABASE,UPDATE,YYYY-MM-DD'
  printf, lun, 'intensity         F10.2        SURFACE,BRIGHTNESS,KRAYLEIGHS'
  printf, lun, 'err_intensity     F10.2        ERROR,INTENSITY,KRAYLEIGHS'
  printf, lun, 'nn_DOWL           F10.2        NEAREST_NEIGHBOR,DOWL,mA'
  printf, lun, 'nn_ew             F10.2        NEAREST_NEIGHBOR,EQUIV_WIDTH,mA'
  printf, lun, 'nn_Dw             F10.2        NEAREST_NEIGHBOR,DOP_WIDTH,mA'
  printf, lun, 'nn_Dw             F10.2        NEAREST_NEIGHBOR,LOR_WIDTH,mA'
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
