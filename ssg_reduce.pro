;+
; $Id: ssg_reduce.pro,v 1.1 2003/06/11 18:05:23 jpmorgen Exp $

; Runs non-interactive SSG reduction

;-

pro ssg_reduce, indir, TV=TV, SHOW=SHOW, PLOT=PLOT

;  ON_ERROR, 2

  if NOT keyword_set(indir) then indir = '/home/jpmorgen/data/ssg/raw'

  ;; Make sure we are a directory, but don't be noisy about it
  spawn, string('test -d ', indir), exit_status=status
  if status ne 0 then return

  dir_array = strsplit(indir, '/', /extract)
  depth = N_elements(dir_array)-1
  rawdepth = where(dir_array eq 'raw', count)
  if count eq 0 then $
    message, 'ERROR: input directory must be in the raw directory tree'

  ;; Discard test, ls-al, etc. and years beyond 2100 ;-)
  if depth - rawdepth eq 1 then begin
     if strlen(dir_array[depth]) ne 4 then return
     if strpos(dir_array[depth], '19') ne 0 $
       and strpos(dir_array[depth], '20') ne 0 then return
  endif

  rawdir=indir
  reddir = ''
  dir_array[rawdepth] = 'reduced'
  for i=0, depth do begin
     reddir = string(reddir, '/', dir_array[i])
  endfor

  ;; Make sure reduced directory exists at this level
  temp = findfile(reddir, count=count)
  if count eq 0 then begin
     spawn, string('mkdir ', reddir)
  endif
  
  ;; Descend to the day dir level
  if depth - rawdepth le 1 then begin
     dirs = findfile(string(indir, '/'))
     for i=0, N_elements(dirs)-1 do begin
        ssg_reduce, string(indir,'/' + dirs[i]), TV=TV, SHOW=SHOW, PLOT=PLOT
     endfor
     return
  endif

  ;; If we made it here, we are a day dir
  ssg_fix_head, rawdir
  ssg_db_init, rawdir, /APPEND, /DELETE
  ssg_raw_cp, rawdir, reddir, /OVERWRITE

  ssg_get_overclock, reddir, TV=TV, /write, /noninteractive

  ssg_biasgen, reddir, TV=TV, PLOT=PLOT
  ssg_biassub, reddir
  ssg_get_sliloc, reddir, TV=TV, SHOW=SHOW, PLOT=PLOT, /write, /noninteractive
  ssg_fit_sliloc, reddir, /noninteractive, /write, /flat_only

  ssg_get_camrot, reddir, TV=TV, SHOW=SHOW, /write, /noninteractive
  ssg_fit_camrot, reddir, /noninteractive, /write

  ssg_lightsub, reddir, TV=TV,SHOW=SHOW
  ssg_flatgen, reddir, TV=TV,SHOW=SHOW
  ssg_flatfield, reddir, TV=TV

  ssg_get_slicer, reddir, /noninteractive, /write, TV=TV, SHOW=SHOW
  ssg_fit_slicer, reddir, /noninteractive, /write

  ssg_get_dispers, reddir, PLOT=PLOT, TV=TV, /noninteractive, /write
  ssg_fit_dispers, reddir, /noninteractive, /write

  ssg_cr_mark, reddir, cutval=5, TV=TV
  ssg_cr_replace, reddir, TV=TV 
  ssg_extract, reddir, SHOW=SHOW, TV=TV, /noninteractive, /write
end

