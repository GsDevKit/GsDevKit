*ob-tools-inspector
basicInspectorNodes
	^ Array streamContents: [ :stream | | ivNames |
		stream nextPutAll: self derivedInspectorNodes]