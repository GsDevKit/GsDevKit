callbacks
_selection: anInteger

	"deselect logic when one item selected in deselectItm"
     (self hasSeveralSelections not and: [selection = anInteger or: [anInteger = 0]]) 
		ifTrue: [ self clearSelection. ^false ].
	"deselect logic when multipeitems selected in listSelectionAt:put:"
	(anInteger ~= 0 and: [ (self multipleSelection at: anInteger ifAbsent: [false]) not ])
		ifTrue:  [
			selection := anInteger.
	    		self multipleSelect: anInteger put: true.
			self listHolder selection: anInteger .
			self changed: #selection. 
     		self signalSelectionChanged.
			switch currentNode: self selectedNode.
			^true ]. 
	^false