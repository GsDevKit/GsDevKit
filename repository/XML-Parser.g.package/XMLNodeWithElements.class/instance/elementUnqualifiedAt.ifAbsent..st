accessing
elementUnqualifiedAt: entityName ifAbsent: aBlock
	elements
		ifNil: [^aBlock value].
	^self elements detect: [:each | each localName = entityName] ifNone: [^aBlock value]