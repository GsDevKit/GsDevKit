configuration
defaultMetaNode
	| class method sender |
	class := OBMetaNode named: 'Class'.
	method := OBMetaNode named: 'References'.
	sender := OBMetaNode named: 'Sender'.
	
	class childAt: #users put: method.
	method displaySelector: #fullNameWithProtocol.
	self chasingBrowsers ifTrue:
		[method childAt: #senders put: sender.
		sender
			displaySelector: #fullNameWithProtocol;
			childAt: #senders put: sender].

	^ class