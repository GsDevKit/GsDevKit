announcing
uninstallStaticHandler

	| exception |
	self registeredSessions removeIfPresent: GsCurrentSession currentSession serialNumber.
	exception := SessionTemps current at: #GemToGemStaticException otherwise: nil.
	System disableSignaledGemStoneSessionError.
 	exception ~~ nil ifTrue: [ Exception removeStaticException: exception ].
