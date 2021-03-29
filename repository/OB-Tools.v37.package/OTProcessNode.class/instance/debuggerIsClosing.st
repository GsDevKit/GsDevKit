updating
debuggerIsClosing

	process == nil ifTrue: [ ^self ].
	process terminate.
	process := nil