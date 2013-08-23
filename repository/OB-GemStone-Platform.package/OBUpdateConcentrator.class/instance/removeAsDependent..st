updating
removeAsDependent: anObject

	| dependentProxy dependent |
	dependentProxy :=  self reverseDependentsMap at: anObject ifAbsent: [ ^nil ].
	anObject removeDependent: dependentProxy.
	self dependentsMap removeKey: dependentProxy.
	self reverseDependentsMap removeKey: anObject.
	^nil