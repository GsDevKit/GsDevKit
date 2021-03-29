filtering
icon: aSymbol forNode: aNode
	| class method |
	class := aNode theClass ifNil: [ ^ #blank ].
	method := class compiledMethodAt: aNode selector ifAbsent: [ ^ #blank ].
	method _allBreakpoints ~~ nil ifTrue: [ ^#breakpoint ].
	(method _selectorPool, method literals) do: [:literal |
		"#halt: is actually an alternate way to generate a userError - equivalent to #error:"
		(literal == #halt or: [literal == #pause ])
			ifTrue: [^#breakpoint].
		(literal == #flag: or: [ literal == #needsWork or: [ literal == #notYetImplemented ] ])
			ifTrue: [ ^ #flag ]].
	 ^ #blank