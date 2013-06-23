Accessing
headers
	^headers ~~ nil
		ifTrue: [headers]
		ifFalse: [
			headers := Dictionary new.
			headers at: 'Mime-Version' put: '1.0'.
			headers at: 'Content-Type' put: self contentType, '; charset="ISO-8859-1"'.
			headers ]
