testing
expectedFailures

	^self enabled 
		ifTrue: [ #( #testExpectedError #testExpectedFailure #testUnexpectedPass) ]
		ifFalse: [ #() ]