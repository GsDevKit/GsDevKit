Accessing
smtpPort
	^smtpPort ~~ nil ifTrue: [ smtpPort ] ifFalse: [ 25 ]
