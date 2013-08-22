private
openClassBrowser
	| classNode |
	classNode := OBClassNode on: (Smalltalk at: pattern first asSymbol).
	^self browser isClassBrowser
		ifTrue: [self browser jumpTo: classNode]
		ifFalse: [classNode browse]