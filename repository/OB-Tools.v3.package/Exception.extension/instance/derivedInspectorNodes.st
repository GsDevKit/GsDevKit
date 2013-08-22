*ob-tools-inspector
derivedInspectorNodes
	^ super derivedInspectorNodes 
		add: (OTDerivedInspectorNode on: self label: 'description' block: [ :obj | obj description ]);
		yourself