*ob-tools-inspector
elementInspectorNodes
	| coll |
	coll := super elementInspectorNodes.
	self size > OTSequenceInspectorNode threshold
		ifTrue: [
			coll add: (OTRangeInspectorNode on: self label: 'First ' start: 1 count: OTSequenceInspectorNode threshold).
			coll add: (OTRangeInspectorNode on: self label: 'Next ' start: 21 count: OTSequenceInspectorNode threshold).
			coll add: (OTRangeInspectorNode on: self label: 'Last ' start: (self size - OTSequenceInspectorNode threshold) count: OTSequenceInspectorNode threshold).
			 coll addAll: ((1 to: OTSequenceInspectorNode threshold) collect: [ :each | OTSequenceInspectorNode on: self index: each ])]
		ifFalse: [ 1 to: self size do: [ :each | coll add: (OTSequenceInspectorNode on: self index: each) ] ].
	^coll