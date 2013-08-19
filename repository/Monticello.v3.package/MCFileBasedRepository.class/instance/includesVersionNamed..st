versions
includesVersionNamed: aString 
	| comparable |
	comparable := ((aString endsWith: '.mcz') and: [ aString size > 4 ])
		ifTrue:
			[ aString
				copyFrom: 1
				to: aString size - 4 ]
		ifFalse: [ aString ].
	^ self allVersionNames includes: comparable