writing xml
endTag: tagName xmlns: xmlns
	self popTag: tagName.
	self stream
		nextPutAll: '</'.
	(xmlns notNil
		and: [xmlns ~= self defaultNamespace])
		ifTrue: [self stream
			nextPutAll: xmlns;
			nextPut: $:].
	stream nextPutAll: tagName.
	self endTag.