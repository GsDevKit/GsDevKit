writing xml
startTag: tagName xmlns: xmlns
	self stream
		nextPut: $<.
	(xmlns notNil
		and: [xmlns ~= self scope defaultNamespace])
		ifTrue: [self stream
			nextPutAll: xmlns;
			nextPut: $:].
	self stream
		nextPutAll: tagName.
	"self canonical
		ifFalse: [self stream space]."
	self pushTag: tagName