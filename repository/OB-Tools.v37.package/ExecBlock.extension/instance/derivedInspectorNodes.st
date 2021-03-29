*ob-tools-inspector
derivedInspectorNodes
	^ super derivedInspectorNodes
		add: (OTDirectReferenceInspectorNode on: self label: 'method' block: [ :obj | _method ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'firstPC' block: [ :obj | iFields1 ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'spare1' block: [ :obj | staticLink ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'numberTemps' block: [ :obj | stkSerialNum ]);
		yourself