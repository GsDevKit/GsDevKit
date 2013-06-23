content
endElement: localName namespace: namespace namespaceURI: uri qualifiedName: qualifiedName
	| currentElement |
	currentElement _ self pop.
	(currentElement namespace isNil
		or: [currentElement namespace = self defaultNamespace])
		ifTrue: [
			currentElement localName asString = localName asString
				ifFalse: [self driver errorExpected: 'End tag "', localName , '" doesn''t match "' , currentElement localName  , '".']]
		ifFalse: [
			currentElement qualifiedName = qualifiedName
				ifFalse: [self driver errorExpected: 'End tag "', qualifiedName , '" doesn''t match "' , currentElement qualifiedName  , '".']]