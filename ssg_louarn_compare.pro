;+
; NAME:
;
; PURPOSE:
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
;
; $Id:$
;
; $Log:$
;-
pro ssg_louarn_compare
  init = {ssg_sysvar}
  
  event_arr = [$
['1996_180',  'Y'], $ 
['1996_251',  'Y'], $ 
['1996_311',  'Y'], $ 
['1996_354',  'Y'], $ 
['1997_051',  'Y'], $ 
['1997_094',  'N'], $ 
['1997_128',  'N'], $ 
['1997_178',  'Y'], $ 
['1997_261',  'N'], $ 
['1997_311',  'N'], $ 
['1997_350',  'N'], $ 
['1998_088',  'N'], $ 
['1998_152',  'N'], $ 
['1998_201',  'N'], $ 
['1998_269',  'N'], $ 
['1998_326',  'N'], $ 
['1999_031',  'N'], $ 
['1999_123',  'N'], $ 
['1999_183',  'Y'], $ 
['1999_224',  'Y'], $ 
['1999_284',  'Y'], $ 
['1999_330',  'N'], $ 
['2000_053',  'Y'], $ 
['2000_142',  'M'], $ 
['2000_364',  'N'], $ 
['2001_289',  'Y']]


  help, event_arr
  ;;sarr = ['1996_180', $
  ;;        '1996_251', $
  ;;        '1996_311', $
  ;;        '1996_354', $
  ;;        '1997_051', $
  ;;        '1997_178', $
  ;;        '1999_183', $
  ;;        '1999_224', $
  ;;        '1999_284', $
  ;;        '2000_053', $
  ;;        '2000_142', $
  ;;        '2001_289']
  for is=0,N_elements(event_arr[0,*])-1 do begin
     ;;s = strsplit(sarr[is], '_', /extract)
     s = strsplit(event_arr[0, is], '_', /extract)
     ;; DOY 1.0 = Jan 1 00:00
     ;; JD Jan 1 12:00 = some integer unless you specify JD
     JD = julday(1, 1, s[0], 0, 0, 0) + s[1] - 1
     nday = JD - !ssg.JDnday0
     caldat, JD, month, day, year, hour
     ;;print, sarr[is], year, month, day, hour
     ;;print, event_arr[*, is], year, month, day
     print, format='(a9, a2, I6, I3, I3, I5)', event_arr[*, is], year, month, day, nday
     
  endfor

end
