*squeak-bootstrap
myDependents
	"Private. Answer a list of all the receiver's dependents."

    | platformSupport |
    (platformSupport := self platformSupport) == nil ifTrue: [ ^#() ].
	^platformSupport dependentsFor: self.
