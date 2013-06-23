accessing
elementAt: entityName ifAbsent: aBlock
	elements
		ifNil: [^aBlock value].
	^self elements detect: [:each | each name = entityName or: [each localName = entityName]] ifNone: [^aBlock value]