accessing
next
	^ self basicNext
		ifNil: [(self hasSelection and: [self shouldBeLast not])
				ifTrue: [self createNext]]