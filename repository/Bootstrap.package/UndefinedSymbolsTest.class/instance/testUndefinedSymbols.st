tests
testUndefinedSymbols
	| undefined |
	self cleanUndefinedSymbols.
	undefined := UndefinedSymbols keys.
	undefined removeAllPresent: self specialCases.
	self assert: undefined isEmpty