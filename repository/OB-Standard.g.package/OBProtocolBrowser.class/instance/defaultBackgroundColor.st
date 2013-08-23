building
defaultBackgroundColor
	| prefs color |
	prefs := Smalltalk at: #Preferences ifAbsent: [^Color lightGreen].
	color _ prefs lexiconWindowColor.
	^ color == false 
		ifTrue: [Color lightGreen]
		ifFalse: [color]