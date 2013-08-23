*ob-tools-inspector
basicInspectorNodes
	<inspector: #'1' priority: 600>
	
	| nodes ivNames |
	nodes :=  self derivedInspectorNodes.
	ivNames := self class allInstVarNames.
	ivNames withIndexDo: [ :name :index |
		nodes add: (OTNamedVariableNode on: self index: index name: name) ].
	1 to: (self basicSize - ivNames size) do: [ :index |
		nodes add: (OTIndexedVariableNode on: self index: index) ].
	^ nodes