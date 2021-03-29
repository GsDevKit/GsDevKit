navigation
filterPragmas
	
	| result |
	result := OrderedCollection new.
	(Pragma
		allNamed: #inspector:priority:
		from: self object class to: nil
		sortedByArgument: 2) do: [ :pragma |
			result add: pragma ].
	^ result