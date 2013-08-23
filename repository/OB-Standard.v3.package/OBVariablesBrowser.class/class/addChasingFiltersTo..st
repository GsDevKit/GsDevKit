private
addChasingFiltersTo: nodes 
	(nodes at: #method)
		childAt: #senders put: (nodes at: #message);
		addFilter: OBModalFilter new.
	(nodes at: #var) addFilter: OBModalFilter new.
	(nodes at: #message)
		displaySelector: #fullName;
		childAt: #senders put: (nodes at: #message);
		addFilter: OBModalFilter new