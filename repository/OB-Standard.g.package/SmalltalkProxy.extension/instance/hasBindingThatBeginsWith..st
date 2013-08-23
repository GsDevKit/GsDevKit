*ob-standard
hasBindingThatBeginsWith: aString
	"Answer true if the receiver has a key that begins with aString, false otherwise
	--DON't know what to do....!--
	self keysDo:[:each | 
		(each beginsWith: aString)
			ifTrue:[^true]]."
	^false