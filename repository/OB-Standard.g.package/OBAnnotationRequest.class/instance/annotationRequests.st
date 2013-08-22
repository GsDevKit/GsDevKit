actions
annotationRequests
	| prefs |
	prefs := Smalltalk at: #Preferences ifAbsent: [ ^OBPlatform current defaultAnnotationRequests ].
	^ prefs defaultAnnotationRequests