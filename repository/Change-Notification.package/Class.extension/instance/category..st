*change-notification
category: newCategory

"Sets the classCategory variable of the receiver.
 The argument should be a kind of CharacterCollection or nil."

| s o |
(s := self _primitiveSize) < Class instSize ifTrue:[
  "Grow an instance that originated in a 4.1 GemStone repository"
  s + 1 to: Class instSize do:[:j | self _unsafeAt: j put: nil ].
  ]. 
newCategory ~~ nil ifTrue:[ newCategory _validateClass: CharacterCollection ].
o := classCategory.
classCategory := newCategory.
ClassOrganizer clearCachedOrganizer.
o ~~ nil 
	ifTrue: [
		SystemChangeNotifier uniqueInstance 
			announce: (ClassMovedAnnouncement itemClass: self oldCategory: o)].

