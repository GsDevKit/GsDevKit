writing
writeMetaclassDefinition: definition
	self chunkContents: [:s | s
		nextPutAll: definition className;
		nextPutAll: ' class';
		cr; tab.
		definition hasClassTraitComposition ifTrue: [ s
			nextPutAll: 'uses: ';
			nextPutAll: definition classTraitCompositionString;
			cr; tab].
		s nextPutAll: 'instanceVariableNames: ''';
		nextPutAll: definition classInstanceVariablesString;
		nextPut: $'.
	]