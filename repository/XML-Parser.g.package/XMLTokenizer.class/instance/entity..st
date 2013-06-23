entities
entity: refName
	^self validating
		ifTrue: [self entities
			at: refName
			ifAbsentPut: [self parseError: 'XML undefined entity ' , refName printString]]
		ifFalse: [DTDEntityDeclaration name: refName value: '']