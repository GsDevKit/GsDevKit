callbacks
doItReceiver
	| class |
	class _ self selectedClass.
	^ class
		ifNotNil: [class theNonMetaClass]
		ifNil: [ nil ]