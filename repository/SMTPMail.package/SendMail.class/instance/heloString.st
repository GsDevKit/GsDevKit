private
heloString

	^ (self class preferenceAt: #useEHLO ifAbsent: [true])
		ifTrue: ['EHLO ']
		ifFalse: ['HELO ']