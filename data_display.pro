FUNCTION DATA_DISPLAY, data
;this procedure will take a structure of data arrays and display them as a sequence of
;plots.
;
;data is a structure of data points. It is designed to be generated from data_select

!P.MULTI = [0, 1, N_TAGS(data)-2]

;check to see if nday data is submitted and convert it to julday
IF TAG_EXIST(data, 'nday') EQ 1b THEN BEGIN
	uts = data.nday
	uts = uts + (julday(1,1,1990,0))

; set up the tag names as an indexed array so it can be used to generate
;table axis names.
	names = TAG_NAMES(data)
	point=[0]
	ut_range=[MIN(uts), MAX(uts)]

;create graphs from the passed in data.
	WHILE (point[0] EQ 0) DO BEGIN
		FOR i=1, (N_TAGS(data)-2) DO BEGIN
			PLOT, uts, data.(i), PSYM=1, YMARGIN=[9, 4], $
			xtickunits = ['hours', 'Days', 'Months', 'Years'], $
			title = names[i], $
			xtitle = names[0], $
			ytitle = names[i], $
			xrange=ut_range
		ENDFOR
	     ;; User selects points
	     message, /CONTINUE, 'Use left and right buttons to zoom in on (bracket) area of interest. Middle button will bring up a menu.'

	     cursor, ut, y, /DOWN, /DATA

	     ;; Left mouse
	     if !MOUSE.button eq 1 then begin
		ut_range[0] = ut
	     endif
	     ;; Right mouse
	     if !MOUSE.button eq 4 then begin
		ut_range[1] = ut
	     endif
	     
		nday = ut - (julday(1,1,1990,0))
		nday_range = ut_range - (julday(1,1,1990,0))

	     ;; Middle mouse
	     if !MOUSE.button eq 2 then begin
		message, /CONTINUE, 'Menu:'
		print, 'exit with selected Range'
		print, 'exit with selected Point'
		print, 'Quit/exit with no selection'
		print, 'unZoom'
		print, 'do Nothing'
		answer = ''
		for ki = 0,1000 do flush_input = get_kbrd(0)
		repeat begin
		   message, /CONTINUE, 'R, P, Q, Z, N'
		   answer = get_kbrd(1)
		   if byte(answer) eq 10 then answer = 'F'
		   for ki = 0,1000 do flush_input = get_kbrd(0)
		   answer = strupcase(answer)
		endrep until $
		  answer eq 'R' or $
		  answer eq 'P' or $
		  answer eq 'Q' or $
		  answer eq 'Z' or $
		  answer eq 'N'

		;; Return all ndays within plot range
		if answer eq 'R' then begin
		   r_idx = where(nday_range[0] le data.nday and data.nday le nday_range[1], $
		                 count)
		   point=data.nday[r_idx]
		endif

		;; Return nday closest to where middle mouse was clicked
		if answer eq 'P' then begin
		   dists = abs(data.nday - nday)
		   junk = min(dists, min_idx, /NAN)
		   count = N_elements(min_idx)
		   point=data.nday[min_idx]
		   point=point + (julday(1,1,1990,0))
		endif

		;; Return -1, setting count=0
		if answer eq 'Q' then begin
		   count = 0
		   point=-1
		endif

		;; unZoom
		if answer eq 'Z' then begin
		   ut_range = [min(uts), max(uts)]
		endif

	     endif ;; Middle mouse


	ENDWHILE
ENDIF ELSE BEGIN
point='please enter a structure that includes nday as a tag'
ENDELSE

Return, point
END
