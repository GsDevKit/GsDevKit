instance creation
metaNode: metaNode root: rootNode selection: selectedNode panels: panels
	| browser |
	browser _ self basicNew initialize.
	panels do: [:ea | browser addPanel: ea].
	browser setMetaNode: metaNode node: rootNode.
	selectedNode ifNotNil: [browser jumpTo: selectedNode].
	^ browser