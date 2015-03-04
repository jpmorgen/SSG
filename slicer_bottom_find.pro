FUNCTION SLICER_BOTTOM_FIND, data_in, Xdata=xdata, LowEnd=lowEnd, HighEnd=highEnd, Narrow=narrow, All=all, Error=error, Right=right

;This script will take an array of data points and return the points
;that are closest to the acceptable deviation. The default is 1 and 10% of max.
;it will return an array of (1%, 10%)

data = data_in

IF KEYWORD_SET(right) THEN data=REVERSE(data)

If KEYWORD_SET(lowEnd) EQ 0 THEN lowEnd = .01
If KEYWORD_SET(highEnd) EQ 0 THEN highEnd = .1
size = N_ELEMENTS(data)

;if xdata isn't given then create a set of integers assuming it is in order
IF N_ELEMENTS(xdata) EQ 0 THEN BEGIN
	xdata=DINDGEN(size)
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
	return, 'sort data first'
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

big_idx = where(data ge high, count)
xten = big_idx[0]


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

;Establish first and second ordere derivatives of the data
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
WHILE d2Data(d2x) LT (MAX(d2Data)*.9) DO BEGIN
	d2x = d2x+1
ENDWHILE 

;This search is basically looking for the first overly drastic change.
;In the first derivative the initial turn on point should be a huge leap.
WHILE d2data(d2x2 + 1) LE d2data(d2x2)*2 DO BEGIN
	d2x2 = d2x2+1
ENDWHILE

;FIlters out the data and only returns the desired data.
;also calculate precision
precision = 0.0
sum = 2.0

IF KEYWORD_SET(Narrow) THEN BEGIN
	IF dfirstpeak LE xTen and dfirstpeak GE xOne THEN Begin
		bounds=dfirstpeak
		precision = (xTen+dfrirstpeak)
		sum=sum+1
	ENDIF
ENDIF ELSE BEGIN
	bounds=xTen
	precision = (xTen-xOne)
	IF d2x2 LE xTen and d2x2 GE xOne THEN BEGIN
		bounds=d2x2
		precision = precision+(d2x2-xOne)
		sum=sum+1
	ENDIF
	IF d2x LE xTen and d2x GE xOne THEN BEGIN
		bounds=d2x
		precision = precision+(d2x-xOne)
		sum=sum+1
	ENDIF
	IF dfirstpeak LE xTen and dfirstpeak GE xOne THEN BEGIN
		bounds=dfirstpeak
		precision=precision+(dfirstpeak-xOne)
		sum=sum+1
	ENDIF	
	IF d2firstpeak LE xTen and d2firstpeak GE xOne THEN BEGIN
		bounds=d2firstpeak
		precision = precision+(d2firstpeak-xOne)
		sum=sum+1
	ENDIF
ENDELSE

deviation=(precision/sum)
IF KEYWORD_SET(All) THEN bounds = [xOne, d2x, dfirstpeak, d2firstPeak, d2x2, xTen, deviation]
IF KEYWORD_SET(error) THEN bound = [bounds, deviation] ELSE bound = bounds

RETURN, bound
END
