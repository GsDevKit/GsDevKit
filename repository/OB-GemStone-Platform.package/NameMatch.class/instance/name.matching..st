as yet unclassified
name: aName matching: aString
	name := aName asSymbol.
	equals := name == aString asSymbol.
	findStringIndex := name findString: aString startingAt: 1 caseSensitive: true.
	findStringNoCaseIndex := name findString: aString startingAt: 1 caseSensitive: false.