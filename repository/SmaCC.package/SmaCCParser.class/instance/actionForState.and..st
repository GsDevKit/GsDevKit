private
actionForState: stateIndex and: aSymbolIndex 
	| index row |
	row := self transitionTable at: stateIndex.
	^(row at: 1) == 2 
		ifTrue: 
			[index := self 
						binarySearchIn: row
						for: aSymbolIndex
						size: 1.
			index == 0 ifTrue: [^self errorAction] ifFalse: [^row at: 2]]
		ifFalse: 
			[index := self 
						binarySearchIn: row
						for: aSymbolIndex
						size: 2.
			index == 0 ifTrue: [^self errorAction] ifFalse: [^row at: index - 1]]