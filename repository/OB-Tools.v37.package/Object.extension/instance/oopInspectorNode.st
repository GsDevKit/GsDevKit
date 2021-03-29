*ob-tools-inspector
oopInspectorNode
	^ OTDerivedInspectorNode on: self label: 'oop' block: [ :obj | obj asOop ]