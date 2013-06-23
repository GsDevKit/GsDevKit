*gemstone-exceptions
baseOverridesGemStoneExceptionsExceptionAClass: known
	self
		addTo: known
		forBehavior: ExceptionA class
		overrides: {
			"2.4.4"
			#'handles:' -> 'handles: anExceptionSelector

	"This message determines whether the ''exception handler'' associted with
	the receiver may be used to process the argument. Answer ''true'' if an 
	associated handler should be used to process ''exception.'' Answer ''false''
	if an associated handler may not be used to process the exception.

	Returns ''true'' if the class of ''exception'' is the receiver or a general
	subclass of the receiver.

	This definition implies that subclasses of an exception class are considered 
	to be ''subexceptions'' of the type of exception defined by their superclass.
	An ''exception handler'' that ''handles'' an exception class will also handle any
	exceptions that are instances of the exception class''s subclasses."

	^self == ExceptionA or: [anExceptionSelector isKindOf: self]
'.
			"2.3.x"
			#'handles:' -> 'handles: anExceptionA

	"This message determines whether the ''exception handler'' associted with
	the receiver may be used to process the argument. Answer ''true'' if an 
	associated handler should be used to process ''exception.'' Answer ''false''
	if an associated handler may not be used to process the exception.

	Returns ''true'' if the class of ''exception'' is the receiver or a general
	subclass of the receiver.

	This definition implies that subclasses of an exception class are considered 
	to be ''subexceptions'' of the type of exception defined by their superclass.
	An ''exception handler'' that ''handles'' an exception class will also handle any
	exceptions that are instances of the exception class''s subclasses."

	^anExceptionA isKindOf: self
'.
			"2.4.4"
			#'category:number:arguments:' -> 'category: cat number: errNum arguments: args

	(cat isKindOf: ExceptionA) ifTrue: [^cat].
	cat == GemStoneError ifTrue: [
		^self 
			gsNumber: 		errNum 
			arguments: 	args.
	].
	^self new
		gsCategory:	cat;
		gsNumber:		errNum;
		gsArguments:	args;
		yourself.
'.
			"2.3.x"
			#'category:number:arguments:' -> 'category: cat number: errNum arguments: args

	cat == GemStoneError ifTrue: [
		^self 
			gsNumber: errNum 
			arguments: args.
	].
	(cat = ExceptionA exceptionCategory and: [
		errNum == ExceptionA errorNumberToSignal]) ifTrue: [
			^args first.
	].
	^self new.
'.
			"2.4.4"
			#'gsNumber:arguments:' -> 'gsNumber: errNum arguments: args

	(errNum >= 2001 and: [errNum <= 2999]) ifTrue: [
	    errNum == 2010 ifTrue: [
			^MessageNotUnderstood new
				message: (FailedMessage new
					arguments: (args at: 3);
					selector: (args at: 2);
					yourself);
				receiver: (args at: 1);
				gsCategory: GemStoneError;
				gsNumber: errNum;
				gsArguments: args;
				yourself.
			].
			errNum == 2026 ifTrue: [
				^ZeroDivide new
					tag: args first;
					gsCategory: GemStoneError;
					gsNumber: errNum;
					gsArguments: args;
					yourself.
			].
		].
	^self new
		gsCategory: GemStoneError;
		gsNumber: errNum;
		gsArguments: args;
		yourself.
'.
			"2.3.x"
			#'gsNumber:arguments:' -> 'gsNumber: errNum arguments: args

	(errNum >= 2001 and: [errNum <= 2999]) 
      ifTrue: [
	    errNum == 2010 ifTrue: [
		    ^MessageNotUnderstood new
			    message: (FailedMessage new
				    arguments: (args at: 3);
				    selector: (args at: 2);
				    yourself);
			    receiver: (args at: 1);
			    yourself.
	    ].
	    errNum == 2026 ifTrue: [
		    ^ZeroDivide new
			    tag: args first;
			    yourself.
	    ]].
	^self new
		gsCategory: GemStoneError;
		gsNumber: errNum;
		gsArguments: args;
		yourself.
'.
	}