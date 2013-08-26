mocks
uuidForName: t1 
	| t2 t3 |
	t2 := t1 asString.
	t3 := '00000000-0000-0000-0000-0000000000'
				, (t2 size = 1
						ifTrue: [t2 , '0']
						ifFalse: [t2]).
	^ UUID fromString: t3