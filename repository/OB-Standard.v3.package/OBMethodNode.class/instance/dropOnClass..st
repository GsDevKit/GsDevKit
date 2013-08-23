drag and drop
dropOnClass: aNode
"	aNode theClass 
		compile: self source
		classified: (self theClass organization 
			categoryOfElement: self selector)."
	aNode theClass compileMethod: self source category: (self theClass categoryOfSelector: self selector).
	"Smalltalk 
		at: #InputSensor 
		ifPresent: [:cl |
			cl default shiftPressed
				ifFalse: [ self theClass removeSelector: self selector ]]."
	aNode signalChildrenChanged.
	^ true