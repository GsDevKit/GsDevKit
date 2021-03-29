*ob-tools-inspector
derivedInspectorNodes
	^ OrderedCollection 
		with: (OTDerivedInspectorNode on: self label: 'self' block: [ :obj | obj ])
		with: (OTDerivedInspectorNode on: self label: 'class' block: [ :obj | obj class ])
		with: (OTDerivedInspectorNode on: self label: 'oop' block: [ :obj | obj asOop ])