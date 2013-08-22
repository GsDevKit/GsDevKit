callbacks
menu: aMenu 
	self isEmpty ifFalse: [self addCommandsToMenu: aMenu].
	^aMenu