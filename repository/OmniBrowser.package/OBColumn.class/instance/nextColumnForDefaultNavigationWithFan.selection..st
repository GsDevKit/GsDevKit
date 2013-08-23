column
nextColumnForDefaultNavigationWithFan: anOBFan selection: aSmallInteger 
	| column |
	column := self next.
	column fan: anOBFan selection: aSmallInteger.
	^ column