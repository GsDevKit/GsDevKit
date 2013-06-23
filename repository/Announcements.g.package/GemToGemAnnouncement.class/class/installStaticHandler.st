announcing
installStaticHandler
	| exception |
	self registeredSessions add: GsCurrentSession currentSession serialNumber.
	exception := Exception
		installStaticException: [ :ex :cat :num :args | 
			GemToGemAnnouncement signal: (args at: 2) message: (args at: 3) sessionSerialNum: (args at: 1).
			System enableSignaledGemStoneSessionError ]
		category: GemStoneError
		number: 6010
		subtype: nil.
	SessionTemps current at: #GemToGemStaticException put: exception.
	System enableSignaledGemStoneSessionError