updating
selectionChanged: ann
	text := ann node ifNil: [String new] ifNotNil: [ann node annotationString].
	self changed: #text