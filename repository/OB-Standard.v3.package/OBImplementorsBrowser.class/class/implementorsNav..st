as yet unclassified
implementorsNav: navSelector
	| selector message implementor |
	selector := OBMetaNode named: 'Selector'.
	message := OBMetaNode named: 'Message'.
	implementor := OBMetaNode named: 'Implementor'.

	selector 
		childAt: navSelector put: implementor;
		addFilter: OBClassSortFilter new.
	implementor
		displaySelector: #fullNameWithProtocol.
	self chasingBrowsers ifTrue:
		[implementor
			childAt: #messages put: message.
		message childAt: navSelector put: implementor].
	^selector