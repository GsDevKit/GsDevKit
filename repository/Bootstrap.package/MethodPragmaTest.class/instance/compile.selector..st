utilities
compile: aString selector: aSelector
	| result source errorString |
	(result := self class 
		compileMethod: (source := aSelector asString , String cr , aString)
		category: self methodCategory
		using: GsSession currentSession symbolList) ~~ nil 
		ifTrue: [
			"Clear the pragmas for the method on a compiler error, colect error info for debugging"
			errorString := GsMethod _sourceWithErrors: result fromString: source.
			self class setPragmas: nil forMethod: aSelector].
	^self class compiledMethodAt: aSelector