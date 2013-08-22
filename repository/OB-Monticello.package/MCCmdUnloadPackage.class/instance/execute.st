execution
execute
	(self confirm: 'Are you sure you want to unload this package?') ~~ true ifTrue: [ ^self ].
	target workingCopy unload.
	self refresh