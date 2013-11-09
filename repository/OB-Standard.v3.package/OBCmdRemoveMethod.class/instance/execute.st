execution
execute
	| senders choice |
	senders := SystemNavigation default allCallsOn: target selector.
	senders isEmpty ifTrue: [^ self doRemove].
	choice := OBChoiceRequest
				prompt: 'This message has ', senders size asString, ' senders.'
				labels: #('Remove it' 
						'Remove, then browse senders' 
						'Don''t remove, but show me those senders' 
						'Forget it -- do nothing -- sorry I asked')
				values: #(doRemove removeAndBrowse browseSenders nil).
	choice ifNotNil: [^ self perform: choice]
