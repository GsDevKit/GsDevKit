actions
removeVersionsForClass: aBehavior

	| cls |
	cls := aBehavior.
	aBehavior isMeta ifTrue: [ cls := aBehavior theNonMetaClass ].
	self dictionary removeKey: cls ifAbsent: [ ].
	self dictionary removeKey: cls class ifAbsent: [ ].
	
