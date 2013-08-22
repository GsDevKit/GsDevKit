filtering
icon: aSymbol forNode: aNode 
	| method |
	method := aNode theClass lookupSelector: aNode selector.
	method ifNil: [ ^ #blank ].
	method _allBreakpoints ~~ nil ifTrue: [ ^#breakpoint ].
	(method _selectorPool, method literals) do: 
		[ :literal | 
		(literal == #halt 
			or: [ literal == #halt: or: [ literal == #pause ] ]) ifTrue: [ ^ #breakpoint ].
		(literal == #flag: or: [ literal == #needsWork ]) ifTrue: [ ^ #needsWork ] ].
	^ aNode isOverride 
		ifTrue: 
			[ aNode isOverridden 
				ifTrue: [ #overriddenAndOverrides ]
				ifFalse: [ #overrides ] ]
		ifFalse: 
			[ aNode isOverridden 
				ifTrue: [ #overridden ]
				ifFalse: [ #blank ] ]