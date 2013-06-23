utilities
assertPragmas: aMethodBody givesKeywords: keywords arguments: arguments
	| pragmas |
	pragmas := self pragmas: aMethodBody.
	1 to: pragmas size do: [:i | | pragma |
		pragma := pragmas at: i.
		self assert: pragma keyword = (keywords at: (i//2) + 1).
		self assert: pragma arguments = (arguments at: (i//2) + 1).
		].