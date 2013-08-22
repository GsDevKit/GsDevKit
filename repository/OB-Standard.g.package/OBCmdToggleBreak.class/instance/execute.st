execution
execute
	(target reference compiledMethod _allBreakpoints ~~ nil)
		ifTrue:
			[target reference compiledMethod clearAllBreaks]
		ifFalse:
			[ target theClass _setMethodBreak: target selector stepPoint: 1 ].
			
	 requestor announce: (OBDefinitionChanged definition: target definition).
	 requestor refresh.