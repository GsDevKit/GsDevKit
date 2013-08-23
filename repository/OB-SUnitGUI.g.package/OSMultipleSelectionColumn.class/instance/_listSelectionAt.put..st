callbacks
_listSelectionAt: anInteger put: aValue 

	(self multipleSelection at: anInteger ifAbsent: [ false ]) ifFalse: [ ^nil ].
	(aValue not and: [self multipleSelection at: anInteger ifAbsent: [false]])
		ifTrue: [ | needsRefresh  |
			"deselection"
			self multipleSelect: anInteger put: false.
			needsRefresh := selection ~= anInteger.
			selection := 0.
			self multipleSelection keysAndValuesDo: [:key :value | value ifTrue: [ selection := key ]].
			self listHolder selection: selection.
			self changed: #selection. 
			self panel columns last == self 
				ifFalse: [ 
					"signal only when there are children to be updated"
					self signalSelectionChanged ].
			switch currentNode: self selectedNode.
			self changed: #multipleSelection.
			needsRefresh ifTrue: [self browser signalRefresh ]].
	^nil