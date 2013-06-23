*squeak-bootstrap
myDependents: aCollectionOrNil
	"Private. Set (or remove) the receiver's dependents list."

    | platformSupport |
    (platformSupport := self platformSupport) == nil ifTrue: [ ^self ].
	aCollectionOrNil
		ifNil: [platformSupport removeDependentsFor: self]
		ifNotNil: [platformSupport putDependentsFor: self collection: aCollectionOrNil].
