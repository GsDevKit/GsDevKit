writing xml
comment: aString
	self startComment.
	self stream nextPutAll: aString.
	self endComment