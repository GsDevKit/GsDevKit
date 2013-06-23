*squeak
compile: methodText classified: cat

	| res |
	res := self compileMethod: methodText
		category: cat
		using: GsSession currentSession symbolList.
	^res ~~ nil
		ifTrue: [ 
			res _error: #compilerErrStDB args: #[methodText].
			nil ]
		ifFalse: [ | sel |
			(sel := Behavior parseSelector: methodText for: self) == nil
				ifTrue: [ sel ]
				ifFalse: [sel asSymbol ]]