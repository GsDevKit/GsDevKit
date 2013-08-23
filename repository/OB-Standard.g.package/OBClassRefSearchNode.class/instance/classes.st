as yet unclassified
classes
	| refs classNames nodes |
	refs := SystemNavigation default allReferencesTo: query asSymbol.
	classNames := refs collect: [:ea | ea classSymbol].
	nodes := classNames asSet collect: [:ea | OBClassNode on: (Smalltalk at: ea)].
	^ nodes asArray sort: [:a :b | a theClassName <= b theClassName]