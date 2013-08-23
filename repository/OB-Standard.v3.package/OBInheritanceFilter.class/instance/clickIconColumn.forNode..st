filtering
clickIconColumn: aColumn forNode: aNode
	| filter implementors choice index |
	filter := OBClassSortFilter new.
	OBWaitRequest block: [
		implementors := filter 
			nodesFrom: aNode hierarchyImplementors
			forNode: aNode ].
	implementors size < 2
		ifTrue: [ ^ super clickIconColumn: aColumn forNode: aNode ].
	index := implementors indexOf: aNode ifAbsent: [ ^ false ].
	choice := OBChoiceRequest
		prompt: 'Select Implementor'
		labels: (implementors collect: [ :each | 
			filter 
				displayString: each theClassName 
				forParent: aNode child: each ])
		values: implementors
		lines: (Array with: index - 1 with: index).
	choice isNil
		ifFalse: [ choice announceSelectionWith: aColumn browser ].
	^ true