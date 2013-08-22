updates
registerClientObjectForwarderDependentFor: anObject

	| clientForwarder |
	true ifTrue: [Smalltalk at: #OBUpdateConcentrator ifPresent: [:cl | ^cl default addDependentFor: anObject ]].
	"old technique"
	clientForwarder := ClientForwarder new.
	anObject dependents isEmpty not ifTrue: [ ^nil ].
	anObject addDependent: clientForwarder.
	^clientForwarder