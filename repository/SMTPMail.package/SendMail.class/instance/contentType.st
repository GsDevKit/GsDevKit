Accessing
contentType
	contentType ifNil: [ contentType := self defaultContentType ].
	^contentType