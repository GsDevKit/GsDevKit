private
refreshMultipleSelections
	"listHolder has different children, need to determine if selectedNodes are still valid and then 
	 reset the selections in the multipleSelection map,
	
	 Return true ihe new node used as primary selection or nil."
	
	| children removed kept nodes newSelection |
	(nodes := self selectedNodes) isEmpty ifTrue: [ ^nil ].
	children := self children.
	removed := Set new.
	kept := Set new.
	nodes do: [:node | | index |
		(index := children indexOf: node) ~~ 0
			ifTrue: [ kept add: index -> node ]
			ifFalse: [ removed add: node ]].
	multipleSelection := Dictionary new.
	selectedNodes := Set new.
	newSelection := 0.
	kept do: [:assoc |
		multipleSelection at: assoc key put: true.
		selectedNodes add: assoc value.
		newSelection := assoc key ].
	^newSelection >0
		ifTrue: [ children at: newSelection ]
	 	ifFalse: [ nil ]