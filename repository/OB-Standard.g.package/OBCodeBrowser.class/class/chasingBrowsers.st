configuration
chasingBrowsers
	| prefs |
	prefs := Smalltalk at: #Preferences ifAbsent: [ ^true ].
	^ prefs chasingBrowsers