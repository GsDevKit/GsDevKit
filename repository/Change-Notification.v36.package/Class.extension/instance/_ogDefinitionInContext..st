*change-notification
_ogDefinitionInContext: aUserProfile

"Returns a description of the receiver using object names taken from the given
 UserProfile. Crafted for OG-Standard browsers"

^ self 
	_ogDefinitionInContext: aUserProfile 
	className: name 
	superClassName: (superClass == nil 
			ifTrue: ['nil'] 
			ifFalse: [superClass name])

