*ob-tools-inspector
elementInspectorNodes
	| result |
	result := super elementInspectorNodes.
	self do: [ :each | result add: (OTSetInspectorNode on: self value: each) ].
	^ result