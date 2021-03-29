*core-squeak
hasInterned: aString ifTrue: symBlock 
	"Answer with false if aString hasn't been interned (into a Symbol), 
	otherwise supply the symbol to symBlock and answer true."

	| sym |
	sym := self _existingWithAll: aString.
	^sym == nil
		ifTrue: [false]
		ifFalse: [symBlock value: sym.  true]