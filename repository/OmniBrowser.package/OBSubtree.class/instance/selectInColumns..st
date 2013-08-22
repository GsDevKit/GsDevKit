as yet unclassified
selectInColumns: aCollection
	| column stream |
	column := aCollection first.
	stream := state readStream.
	stream atEnd ifFalse:
		[column fan: stream next selection: stream next].
	[stream atEnd] whileFalse:
		[(column nextColumnWithFan: stream next selection: stream next)
		     ifNil: [column := column next] ifNotNilDo: [:col | column := col]].