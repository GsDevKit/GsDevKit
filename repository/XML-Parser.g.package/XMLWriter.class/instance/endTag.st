writing xml
endTag
	self stream nextPutAll: '>'.
	self indent.
	"self canonical
		ifFalse: [self stream space]"