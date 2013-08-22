testing
isActive

	| column |
	super isActive ifFalse: [ ^false ].
	column := requestor browser navigationPanel columns at: 2.
	^column selectedNode notNil