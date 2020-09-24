message list
visibleMethods
	^ self selectedClassName
		ifNil: [#()]
		ifNotNil: [self methodsForSelectedProtocol]