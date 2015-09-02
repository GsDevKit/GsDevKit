navigation
filters
	"Answer a colletion of filters (label to elements) for the receiving object."
	
	| result items |
	result := OrderedCollection new.
	(Pragma
		allNamed: #inspector:priority:
		from: self object class to: nil
		sortedByArgument: 2) do: [ :pragma |
			items := self object perform: pragma selector.
			items isEmptyOrNil ifFalse: [ 
				items do: [ :each | each metaNode: metaNode ].				
				result add: (pragma argumentAt: 1) -> items ] ].
	^ result