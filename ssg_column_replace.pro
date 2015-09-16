FUNCTION ssg_column_replace, Newim

;This procedure will replace all the bad columns with non-nan values for rotation.
;It will also generate a file that will hold a mapping of all the the nan values so that
;when derotation is complete we can replace all the nan values. We don't want to have
;fabricated data in our final analysis.


im = Newim
imSize = SIZE(im)
XElements = imSize[1]
YElements = imSize[2]
IF (imSize[0] EQ 1) THEN middle = 0 ELSE middle = ROUND(YElements/2)
map = MAKE_ARRAY(XElements, YElements, value=0)
firstRow = 1
SegmentCounter = 1
firstFinite = 1

for i=0, XElements-1 DO BEGIN

	IF (finite(im[i, middle]) EQ 0 and i EQ 0) THEN BEGIN
		map[i, *] = 1
		firstFinite = 0
		CONTINUE
	ENDIF
	IF (finite(im[i, middle]) EQ 0 and firstFinite EQ 0) THEN BEGIN
		map[i, *] = 1
		CONTINUE
	ENDIF
	IF (finite(im[i, middle]) EQ 1 and firstFinite EQ 0) THEN BEGIN
		map[i, *] = 0
		firstFinite = 1
		CONTINUE
	ENDIF
	IF (finite(im[i, middle]) EQ 0 and firstFinite EQ 1) THEN BEGIN
		map[i, *] = 1
		FirstRow = 0
		SegmentCounter = SegmentCounter+1
		CONTINUE
		 
	ENDIF ELSE BEGIN
		map[i, *] = 0 	
		IF(firstrow EQ 0) THEN BEGIN
			SegmentSize = SegmentCounter+1
			imSegment = MAKE_ARRAY(SegmentSize, YElements)
			FOR j=1, segmentSize DO BEGIN
				imSegment[segmentSize-J, *] = im[i+1-j, *]
			ENDFOR
			segmentToInsert = SMOOTH(imSegment, [segmentSize-1, 1], /NAN)
			FOR j=1, segmentSize DO BEGIN
				im[i+1-j, *] = segmentToInsert[segmentSize-j, *]
			ENDFOR
			firstRow = 1
			SegmentCounter = 1
		ENDIF
		
	ENDELSE
ENDFOR
toReturn = {toReturn, image:im, map:map}
Return, toReturn

END
