command
execute
	| messagesSent messageNode |
	messagesSent := target selectorAndMessages.
	messagesSent size = 1
		ifFalse: 
			[ messageNode := OBChoiceRequest 
				prompt: nil
				labels: (messagesSent collect: [:ea | ea name])
				values: messagesSent
				lines: #(1 ).
			 messageNode ifNil: [ ^ self ] ]
		ifTrue: [messageNode := messagesSent first].
	(requestor announce: (self announcementClass of: messageNode unifySelectors)) execute