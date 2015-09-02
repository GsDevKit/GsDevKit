*ob-tools-inspector
protocolInspectorNodes
	<inspector: #'#' priority: 800>

	^ self class allSelectors asArray sortAscending collect: [ :each | OTProtocolInspectorNode on: self selector: each ]