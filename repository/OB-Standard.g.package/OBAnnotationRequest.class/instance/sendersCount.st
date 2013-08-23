requests
sendersCount
	| sendersCount |
	sendersCount := (self systemNavigation allCallsOn: selector) size.
	sendersCount := sendersCount == 1
						ifTrue: ['1 sender']
						ifFalse: [sendersCount printString , ' senders'].
	^ sendersCount