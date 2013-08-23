accessing
addCommandsOn: aNode for: aRequestor to: aSet
	| cmd |
	factories do: 
		[:ea | 
		cmd _ ea on: aNode for: aRequestor.
		cmd isActive ifTrue: [aSet add: cmd]]