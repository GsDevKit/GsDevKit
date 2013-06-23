*gemstone-exceptions
gsNumber: errNum arguments: args

	| errorClass |
	(errNum >= 1001 and: [errNum <= 1999]) ifTrue: [errorClass := CompilerError		].
	(errNum >= 2001 and: [errNum <= 2999]) 
      ifTrue: [
	    errNum == 2010 ifTrue: [
		    ^MessageNotUnderstood new
				gsArguments: args;
			    message: (FailedMessage new
				    arguments: (args at: 3);
				    selector: (args at: 2);
				    yourself);
			    receiver: (args at: 1);
			    yourself.
	    ].
	    errNum == 2026 ifTrue: [
		    ^ZeroDivide new
				gsCategory: GemStoneError;
			    tag: args first;
			    yourself.
	    ].
	    errNum == 2318 ifTrue: [
			((args at: 1) == #halt and: [(args at: 2) = '#halt encountered']) ifTrue: [ ^Halt new ].
			^UserDefinedError new
				gsCategory: GemStoneError;
				messageText: (args at: 2);
				gsArguments: args;
				yourself.
	    ].
	    errNum == 2322 ifTrue: [
			^ErrCantBecomeSelfOnStack new
				gsCategory: GemStoneError;
				tag: args first;
				yourself.
	    ].
        errorClass := InterpreterError.
	].
	(errNum >= 3001 and: [errNum <= 3999]) ifTrue: [errorClass := AbortingError		].
	(errNum >= 4001 and: [errNum <= 4999]) ifTrue: ["Fatal Error range, but by definition, Fatal errors cannot be handled, so we should never get here."].
	(errNum >= 6001 and: [errNum <= 6999]) 
      ifTrue: [
	    errNum == 6001 ifTrue: [
		    ^Halt new
				gsCategory: GemStoneError;
				yourself.
	    ].
       errorClass := ControlInterrupt.
	].
	errorClass isNil ifTrue: [^self new].
	^errorClass new
		gsCategory: GemStoneError;
		gsNumber: errNum;
		gsArguments: args;
		yourself.
