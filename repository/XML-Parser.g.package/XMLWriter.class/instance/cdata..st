writing xml
cdata: aString
	self startCData.
	self stream nextPutAll: aString.
	self endCData