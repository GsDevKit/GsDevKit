navigation
childFilterPragmas
	
	| result |
	result := OrderedCollection new.
	[(Pragma
		allNamed: #inspector:priority:
		from: self value class to: nil
		sortedByArgument: 2) do: [ :pragma |
			result add: pragma ]] on: Error, Halt do: [:ex | ^#() ].
	^ result