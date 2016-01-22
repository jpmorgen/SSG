function BAD_SPECTRUM_FIND, nday_start, nday_end

  dbclose ;; Just in case
  dbname = 'ssg_reduce'
  dbopen, dbname, 0

  if N_elements(nday_start) eq 0 then $
     nday_start = 0

  if N_elements(nday_end) eq 0 then $
     nday_end = 36500

	
  ndayl = string(format='("nday>", i5)', nday_start)
  ndayh = string(format='("nday<", i5)', nday_end)

  entries = dbfind(ndayl, $
                   dbfind(ndayh, $
                          dbfind("bad<2047", $
                                 dbfind("typecode=2"))), $
                   count=data_count)

  dbext, entries, "fname, nday, date, bad, m_dispers", $
         files, ndays, dates, badarray, disp_arrays
		
		FOR ifile=0, data_count-1 DO BEGIN
		    disp = disp_arrays[*,ifile]
		    IF N_elements(c0) EQ 0 then BEGIN
		       c0 = disp[0] 
			c1 = disp[1] 
			c2 = disp[2] 
		ENDIF ELSE BEGIN
		       c0 = [c0, disp[0]] 
			c1 = [c1, disp[1]] 
			c2 = [c2, disp[2]] 
		ENDELSE
		ENDFOR ;; each dispersion

IF N_ELEMENTS(c0) GT 0 THEN BEGIN
	tags = ['nday', 'c0', 'c1', 'c2', 'metadata']
	toReturn = CREATE_STRUCT(tags, ndays, c0, c1, c2, {database:'ssg_reduce', version:systime()})
ENDIF ELSE BEGIN
	print, 'No Data'
ENDELSE
dbclose
return, toReturn
END
