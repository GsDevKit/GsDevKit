accessing columns
currentColumn
	^self columns reversed detect: [:ea | ea hasSelection] ifNone: []