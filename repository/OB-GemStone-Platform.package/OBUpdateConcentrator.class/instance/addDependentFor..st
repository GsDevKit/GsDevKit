updating
addDependentFor: anObject

	| dependentProxy clientForwarder |
	anObject dependents isEmpty not ifTrue: [ ^nil ].
	clientForwarder := ClientForwarder new.
	dependentProxy := OBDependentProxy for: self.
	anObject addDependent: dependentProxy.
	self dependentsMap at: dependentProxy put: clientForwarder.
	self reverseDependentsMap at: anObject put: dependentProxy. 
	^clientForwarder