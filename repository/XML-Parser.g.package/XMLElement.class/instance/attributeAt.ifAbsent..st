accessing
attributeAt: attributeName ifAbsent: aBlock
	^self attributes at: attributeName ifAbsent: [^aBlock value]