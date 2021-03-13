as yet unclassified
buggyInspectorNodes
	<inspector: #'TEST' priority: 300>

	| nodes index |
	nodes := super basicInspectorNodes.
	index := self class allInstVarNames  size.
	nodes add: (OTNamedVariableNode on: self index: index + 1 name: 'bogus instance variable') .
	index := self basicSize.
	nodes add: (OTIndexedVariableNode on: self index: index + 1).
	nodes add: (OTDictionaryInspectorNode on: self key: 'bogusKey').
	index := self basicSize.
	nodes add: (OTSequenceInspectorNode on: self index: index + 1).
	nodes add: (OTDerivedInspectorNode on: self label: 'bogus derived (error)' block: [:obj | self error: 'error''']).
	nodes add: (OTDerivedInspectorNode on: self label: 'bogus derived (halt)' block: [:obj |self halt]).
	^ nodes