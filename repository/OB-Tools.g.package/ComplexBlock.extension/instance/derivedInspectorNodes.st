*ob-tools-inspector
derivedInspectorNodes
	^ super derivedInspectorNodes
		add: (OTDirectReferenceInspectorNode on: self label: 'selfValue' block: [ :obj | selfValue ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'originalInstance' block: [ :obj | originalInstance ]);
		add: (OTDirectReferenceInspectorNode on: self label: 'staticLink' block: [ :obj | staticLink ]);
		yourself