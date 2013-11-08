as yet unclassified
sendersNav: navSelector
	| selector sender |
	selector := OBMetaNode named: 'Selector'.	
	sender := OBMetaNode named: 'Sender'.
	selector 
		childAt: navSelector put: sender;
		addFilter: OBRescueFilter new.
	sender 
		displaySelector: #fullNameWithProtocol;
		addFilter: OBRescueFilter new.
	self chasingBrowsers ifTrue: [sender childAt: navSelector put: sender].
	^ selector	