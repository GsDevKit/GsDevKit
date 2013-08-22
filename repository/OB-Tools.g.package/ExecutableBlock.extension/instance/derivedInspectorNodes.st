*ob-tools-inspector
derivedInspectorNodes
	^ super derivedInspectorNodes
		add: (OTDirectReferenceInspectorNode on: self label: 'method' block: [ :obj | method ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'firstPC' block: [ :obj | firstPC ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'spare1' block: [ :obj | spare1 ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'numberArgs' block: [ :obj | numberArgs ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'numberTemps' block: [ :obj | numberTemps ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'firstSourceOffset' block: [ :obj | firstSourceOffset ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'lastSourceOffset' block: [ :obj | lastSourceOffset ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'argsAndTemps' block: [ :obj | argsAndTemps ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'blockSelfUsed' block: [ :obj | blockSelfUsed ]);
		yourself