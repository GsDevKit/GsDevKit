*gemstone-exceptions
category: cat number: errNum arguments: args

	(cat isKindOf: ExceptionA) ifTrue: [^cat].
	cat == GemStoneError ifTrue: [
		^self 
			gsNumber: 		errNum 
			arguments: 	args.
	].
	(cat = ExceptionA exceptionCategory and: [
		errNum == ExceptionA errorNumberToSignal]) ifTrue: [
			^args first
				gsCategory: 		cat;
				gsNumber: 			errNum;
				gsArguments: 	args;
				yourself.
	].
	^self new
		gsCategory:	cat;
		gsNumber:		errNum;
		gsArguments:	args;
		yourself.
