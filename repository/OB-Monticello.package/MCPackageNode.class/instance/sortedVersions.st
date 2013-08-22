accessing
sortedVersions
	| result |
	result := self versions asSortedCollection: [:a :b | a versionNumber >= b versionNumber ].
	^result := result 
		collect: [ :node | node name: (self versionHighlight: node name asString) ]