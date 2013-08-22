configuration
annotationPanel
	^ OBPlatform current annotationPanes 
		ifTrue: [ OBAnnotationPanel new ]
		ifFalse: [ nil ]