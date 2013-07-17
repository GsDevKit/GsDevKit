testing
methodsInCategory: aString ofClass: aClass 
	^ ((aClass selectorsIn: aString) ifNil: [#()])
			collect: [:sel | self referenceForMethod: sel ofClass: aClass]