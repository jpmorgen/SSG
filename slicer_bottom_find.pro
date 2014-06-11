FUNCTION SLICER_BOTTOM_FIND, data, Xdata=xdata, LowEnd=lowEnd, HighEnd=highEnd, Narrow=narrow, All=all

;This script will take an array of data points and return the points
;that are closest to the acceptable deviation. The default is 1 and 10% of max.
;it will return an array of (1%, 10%)

If KEYWORD_SET(lowEnd) EQ 0 THEN lowEnd = .01
If KEYWORD_SET(highEnd) EQ 0 THEN highEnd = .1
size = N_ELEMENTS(data)

;if xdata isn't given then create a set of integers assuming it is in order
IF N_ELEMENTS(xdata) EQ 0 THEN BEGIN
	xdata=FINDGEN(size)
ENDIF

;make sure the data is ordered by x coordinate, probably time 
;set a boolean if not in order
dataElements = size
notInOrder=0
FOR i=0, dataElements-2 Do Begin
	if xdata(i) GT xdata(i+1) then notInOrder=1 
EndFOR

IF notInOrder EQ 1 THEN BEGIN
	Print, 'Sort data first'
	Return, 'Sort data first'
ENDIF

;adjust the data down so that it will always return a useable result.
;effectively acts as a smoother.
IF MIN(data) NE 0 THEN data=data-MIN(data)

;Find the first peak, set a min value and max value and then start homing in
;on the point of increase
max = MAX(data)
low=max*lowEnd
high=max*highEnd
xTen = 0
xOne = 0

;find the first x coordinate that corresponds with the high end guess
WHILE data(xTen) LE high DO BEGIN
	xTen = xTen+1
	IF data(xTen) EQ low THEN xOne = xTen
ENDWHILE

;Work backwords from the high end guess to find the first low end guess
IF data(xOne) NE low THEN BEGIN
	xOne = xTen
	WHILE data(xOne - 1) GE low DO BEGIN
		xOne = xOne-1
	ENDWHILE
ENDIF

;Establish dirst and second ordere derivatives of the data
dData = DERIV(data)
d2Data = DERIV(dData)
d2x2 = 0
d2x=0

;Finds the first peak of the first and second derivatives which is the
;simplest and most accurate case in most instances
dfirstpeak = FIRST_PEAK_FIND(dData, 'left')
d2firstPeak = FIRST_PEAK_FIND(d2Data, 'left')

;this works throgh the data to find the first instance of the max
;the idea here being that the first peak might be a local max
;that is throwing off the data
WHILE d2Data(d2x) LT MAX(d2Data) DO BEGIN
	d2x = d2x+1
ENDWHILE 

;This search is basically looking for the first overly drastic change.
;In the first derivative the initial turn on point should be a huge leap.
WHILE d2data(d2x2 + 1) LE d2data(d2x2)*2 DO BEGIN
	d2x2 = d2x2+1
ENDWHILE

;FIlters out the data and only returns the desired data.
IF KEYWORD_SET(Narrow) THEN BEGIN
	IF dfirstpeak LE xTen and dfirstpeak GE xOne THEN bounds=dfirstpeak
ENDIF ELSE BEGIN
	bounds=xTen
	IF d2x2 LE xTen and d2x2 GE xOne THEN bounds=d2x2
	IF d2x LE xTen and d2x GE xOne THEN bounds=d2x
	IF dfirstpeak LE xTen and dfirstpeak GE xOne THEN bounds=dfirstpeak
	IF d2firstpeak LE xTen and d2firstpeak GE xOne THEN bounds=d2firstpeak
ENDELSE

IF KEYWORD_SET(All) THEN bounds = [xOne, d2x, dfirstpeak, d2firstPeak, d2x2, xTen]

RETURN, bounds
END

