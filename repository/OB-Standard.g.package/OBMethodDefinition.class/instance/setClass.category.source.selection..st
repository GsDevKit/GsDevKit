initializing
setClass: aClass category: aString source: aText selection: anInterval
	theClass := aClass.
	category := aString.
	source := aText.
	selection := anInterval.
	callback := [:sel | OBMethodNode on: sel inClass: theClass]