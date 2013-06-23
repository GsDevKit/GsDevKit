writing xml
endEmptyTag: tagName
	self popTag: tagName.
	self stream nextPutAll: '/>'.
	self canonical
		ifFalse: [self stream space]