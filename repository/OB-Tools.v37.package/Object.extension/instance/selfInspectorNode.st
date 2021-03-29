*ob-tools-inspector
selfInspectorNode
	^ OTDerivedInspectorNode on: self label: 'self' block: [ :obj | obj ]