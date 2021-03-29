*change-notification
baseOverridesChangeNotificationClass: known
	self
		addTo: known
		forBehavior: Class
		overrides: {
			#'category:' -> 'category: newCategory

"Sets the classCategory variable of the receiver.
 The argument should be a kind of CharacterCollection or nil."

| s |
(s := self _primitiveSize) < Class instSize ifTrue:[
  "Grow an instance that originated in a 4.1 GemStone repository"
  s + 1 to: Class instSize do:[:j | self _unsafeAt: j put: nil ].
  ]. 
newCategory ~~ nil ifTrue:[ newCategory _validateClass: CharacterCollection ].
classCategory := newCategory 
'.
			#'definition' -> 'definition

"Returns a String containing a GemStone Smalltalk definition for the receiver
 (that is, a subclass creation message).  This method uses the UserProfile
 of the owner of the current session as the correct context."

"For use with the Topaz run command."

^ self _definitionInContext: System myUserProfile
'.
	}