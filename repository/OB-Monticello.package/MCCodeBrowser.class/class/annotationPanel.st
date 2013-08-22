as yet unclassified
annotationPanel
	^ OBPlatform current annotationPanes 
		ifTrue: [ OBAnnotationPanel new ]
		ifFalse: [ nil ]