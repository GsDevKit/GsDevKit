entities
externalEntity: refName
	^self entities
		at: refName
		ifAbsentPut: ['']