printing
printOn: aStream 
	aStream
		nextPutAll: selector printString;
		nextPutAll: '->'.
	metaNode shortPrintOn: aStream