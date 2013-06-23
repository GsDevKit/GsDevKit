*core-squeak
_recompile: aSelector

	| stamp result source |
	stamp := self stampForMethod: aSelector.
	source := self sourceCodeAt: aSelector.
	[ result := self 
		compileMethod: source
		category: (self categoryOfSelector: aSelector)
		using: GsSession currentSession symbolList ] on: Warning do: [:ex | ex resume ].
	result ~~ nil
		ifTrue: [ | errorString |
			errorString := GsMethod _sourceWithErrors: result fromString:  source.
			^self error: errorString ].
	self setStamp: stamp forMethod: aSelector