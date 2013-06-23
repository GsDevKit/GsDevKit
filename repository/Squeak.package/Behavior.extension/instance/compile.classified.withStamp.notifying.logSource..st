*squeak
compile: methodText classified: cat withStamp: aStamp notifying: ignored logSource: ignored2

	| result sel |
	result := self compileMethod: methodText
		category: cat
		using: GsSession currentSession symbolList.
	result ~~ nil
		ifTrue: [ | errorString |
			 errorString := GsMethod _sourceWithErrors: result fromString:  methodText.
			^self error: errorString].
	sel := Behavior parseSelector: methodText for: self.
	self setStamp: aStamp forMethod: sel.
	^nil