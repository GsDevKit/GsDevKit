*gemstone-exceptions
baseOverridesGemStoneExceptionsExceptionA: known
	self
		addTo: known
		forBehavior: ExceptionA
		overrides: {
			"2.4.4"
			#'gsCategory' -> 'gsCategory

	^gsCategory.
'.
			"2.3.x"
			#'gsCategory' -> 'gsCategory

	^gsCategory ~~ nil
		ifTrue:  [gsCategory]
		ifFalse: [self class exceptionCategory].
'.
			"2.4.4"
			#'description' -> 'description
	"Return text that describes in a human readable form 
	an occurrence of an exception. If an explicit message
	text was provided by the signaler of the exception,
	that text should be incorporated into the description."

	| cat array num desc args |
	messageText ~~ nil ifTrue: [^messageText].
	(cat := self gsCategory) isNil ifTrue: [^self genericDescription].
	(cat isKindOf: SymbolDictionary) ifFalse: [^self genericDescription].
	array := cat 
		at: System myUserProfile nativeLanguage 
		ifAbsent: [^self genericDescription].
	(num := self gsNumber) isNil ifTrue: [^self genericDescription].
	array size < num ifTrue: [^self genericDescription].
	(desc := array at: num) isNil ifTrue: [^self genericDescription].
	((args := self gsArguments) isKindOf: Array) ifFalse: [^self genericDescription].
	^self gsCategory 
		textForError: num
		args: args.
'.
			"2.3.x"
			#'description' -> 'description
	"Return text that describes in a human readable form 
	an occurrence of an exception. If an explicit message
	text was provided by the signaler of the exception,
	that text should be incorporated into the description."

	| cat array num desc args |
	messageText ~~ nil ifTrue: [^messageText].
	(cat := self gsCategory) isNil ifTrue: [^self genericDescription].
	array := cat 
		at: System myUserProfile nativeLanguage 
		ifAbsent: [^self genericDescription].
	(num := self gsNumber) isNil ifTrue: [^self genericDescription].
	array size < num ifTrue: [^self genericDescription].
	(desc := array at: num) isNil ifTrue: [^self genericDescription].
	((args := self gsArguments) isKindOf: Array) ifFalse: [^self genericDescription].
	^self gsCategory 
		textForError: num
		args: args.
'.
	}