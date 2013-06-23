*gemstone-exceptions
description
	"Return text that describes in a human readable form 
	an occurrence of an exception. If an explicit message
	text was provided by the signaler of the exception,
	that text should be incorporated into the description."

	| cat msg args |
	messageText ~~ nil ifTrue: [ ^messageText ].
	(cat := self gsCategoryForDescription) isNil ifTrue: [ ^self genericDescription ].
	(cat isKindOf: SymbolDictionary) ifFalse: [ ^self genericDescription ].
	msg := self _messageForErrorNum: self gsNumber withArgs: self gsArguments inLanguage: #English.
	args := gsArguments == nil
		ifTrue: [ nil ]
		ifFalse: [gsArguments copyReplacing: self with: self class].
	^msg == nil
		ifTrue: [ self class name, ' ' , self gsNumber printString,  ' with arguments contained in ', args printString, ' was thrown. ' ]
		ifFalse: [ self class name, ' ' , self gsNumber printString,  ': ', msg ]