as yet unclassified
setUp
	super setUp.
	expected := self mockVersion.
	self change: #one toReturn: 2.