entities
parameterEntity: refName
	^self parameterEntities
		at: refName
		ifAbsent: [self parseError: 'XML undefined parameter entity ' , refName printString]