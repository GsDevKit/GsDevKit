*squeak
compiledMethodAt: aSelector ifAbsent: aBlock
	"Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, return the value of aBlock"

| aSym meth |
aSym := Symbol _existingWithAll: aSelector.
aSym == nil ifTrue:[ ^ methodDict _errorKeyNotFound: aSelector ].
meth := GsPackagePolicy current compiledMethodAt: aSym for: self.
meth ~~ nil ifTrue: [ ^meth ].
^ methodDict at: aSym ifAbsent: aBlock