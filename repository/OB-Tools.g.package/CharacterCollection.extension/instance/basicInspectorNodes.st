*ob-tools-inspector
basicInspectorNodes
	| nodes ivNames |
	nodes := OrderedCollection new.
	nodes addAll: self derivedInspectorNodes.
	ivNames := self class allInstVarNames.
	ivNames withIndexDo: [ :name :index |
		nodes add: (OTNamedVariableNode on: self index: index name: name) ].
	1 to: (self size - ivNames size) do: [ :index |
		nodes add: (OTIndexedVariableNode on: self index: index) ].
	^nodes