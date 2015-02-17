FUNCTION DATA_SELECT, xName, XBot=xBot, XTop=xTop, YParams=yParams, base=database

;This program will take in information regarding the intended x coordinate and y 
;coordinates and return an array of pointers which point to the vectors that contain
;the database information. The first element of the array will point to the x axis
;and the subsequent elements will contain the other vectors in the order they were
;passed
;
;Accepts
;XName is the name of the x parameter
;XBot is the lower most boundry to be searched
;XTop is the upper most boundry to be searched
;Yparams is all the parameters you want extracted
;
;Returns a structure toReturn with the tag names the same as the argument paramaters


;Determines if there is a database and sets default to SSG if there isn't
if N_Elements(base) NE 0 THEN BEGIN 
	dbname=base
	ENDIF ELSE BEGIN
	dbname='ssg_reduce'
ENDELSE
dbopen, dbname, 0

;This is just to define a single input string for the dbfind procedure
if N_Elements(xName) EQ 0 THEN xName='nday'
finder=xName

;if xbot is defined will convert it to a string and add it to the finder string
if N_Elements(xBot) NE 0 THEN BEGIN 
	if SIZE(xBot, TYPE=1) NE 7 THEN BEGIN
		xBot=STRTRIM(xBot, 2)
	ENDIF
	finder=xBOT+'<'+finder 
ENDIF

;if xTop is defined will convert it to a string and add it to the finder string
if N_Elements(xTop) NE 0 THEN BEGIN 
	if SIZE(xTop, TYPE=1) NE 7 THEN BEGIN
		xTop=STRTRIM(xTop, 2)
	ENDIF
	finder=finder+'<'+xTop 
ENDIF

;uses finder to define the database parameters we want to extract
entries = dbfind(finder)

;If there are no y paramaters entered will default to slicer top and bottom
if N_Elements(yParams) EQ 0 THEN BEGIN
	yParams=['sli_bot', 'sli_top']
ENDIF

;this section generates the string required to use dbext
;starts by defining a variable and setting and including the xname
toExt=xName

;paramNumber is used to later determine how many output variables are needed for dbext
paramNumber=N_Elements(yParams)+1
tags=MAKE_ARRAY(paramNumber+1, /string)
tags[0]=xName

FOR I=1, paramNumber-1 DO BEGIN
	tags[i]=yParams[i-1]
ENDFOR
tags[paramNumber]='metadata'

;This loop goes through and adds all the y paramaters to the string used by dbext
for i=0, N_Elements(yParams)-1 DO BEGIN
	toExt=toExt+', '+yParams[i]
ENDFOR

;Case statement determines the number of output variables required by dbext and 
;executes the dbext using toExt defined as the sum of all the variable name strings
;generates an array of pointer variables which point to the arrays containing the 
;extracted data
CASE paramNumber of
	1: BEGIN
		 dbext, entries, toExt, xCor
		 toReturn=CREATE_STRUCT(tags, xcor, {metadata, database:dbname, version:SYSTIME()})
	END
	2: Begin
		dbext, entries, toExt, xCor, yOne
		toReturn=CREATE_STRUCT(tags, xcor, yOne, {metadata, database:dbname, version:SYSTIME()})
	END
	3: BEGIN
		dbext, entries, toExt, xCor, yOne, yTwo
		toReturn=CREATE_STRUCT(tags, xcor, yOne, yTwo, {metadata, database:dbname, version:SYSTIME()})
	END
	4: BEGIN
		dbext, entries, toExt, xCor, yOne, yTwo, yThree
		toReturn=CREATE_STRUCT(tags, xcor, yOne, yTwo, yThree, {metadata, database:dbname, version:SYSTIME()})
	END
	5: Begin
		dbext, entries, toExt, xCor, yOne, yTwo, yThree, yFour
		toReturn=CREATE_STRUCT(tags, xcor, yOne, yTwo, yThree, yFour, {metadata, database:dbname, version:SYSTIME()})
	END
	6: Begin
		dbext, entries, toExt, xCor, yOne, yTwo, yThree, YFour, yFive
		toReturn=CREATE_STRUCT(tags, xcor, yOne, yTwo, yThree, yFour, yFive, {metadata, database:dbname, version:SYSTIME()})
	END
	Else: PRINT, 'inappropriate number of parameters'
ENDCASE

dbclose
;returns pointer array
RETURN, toReturn
END
