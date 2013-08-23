instance creation
onClass: aClass selector: aSelector
	^ self selection: (OBMethodNode on: aSelector inClass: aClass)