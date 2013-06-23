*gemstone-exceptions
handlesCategory: anObject number: anInteger

	^(ControlInterrupt handlesCategory: anObject number: anInteger) not and: [
		super handlesCategory: anObject number: anInteger.
	].
