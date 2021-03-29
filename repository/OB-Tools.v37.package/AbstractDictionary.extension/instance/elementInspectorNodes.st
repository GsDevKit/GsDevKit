*ob-tools-inspector
elementInspectorNodes
	| result |
	result := super elementInspectorNodes first: 2.
	self keysDo: [ :each | result add: (OTDictionaryInspectorNode on: self key: each) ].
	^ result
