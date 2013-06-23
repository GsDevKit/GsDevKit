signalling
broadcast

	| badSerialNumbers |
	badSerialNumbers := Array new.
	self registeredSessions do: [:aSessionSerialNumber |
		[System _sendSignal: self signalNumber toSess: aSessionSerialNumber withMessage: self signalMessage]
			on: Error
			do: [:ex | 
				"Gem is no longer around or no longer listening"
				badSerialNumbers add: aSessionSerialNumber.
				ex return]].
	self registeredSessions removeAll: badSerialNumbers
