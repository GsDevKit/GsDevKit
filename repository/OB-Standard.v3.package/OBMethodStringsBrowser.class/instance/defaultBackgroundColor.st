as yet unclassified
defaultBackgroundColor
	| prefs color |
	prefs := Smalltalk at: #Preferences ifAbsent: [^super defaultBackgroundColor].
	color _ prefs messageListWindowColor.
	^ color == false 
		ifTrue: [super defaultBackgroundColor]
		ifFalse: [color]
