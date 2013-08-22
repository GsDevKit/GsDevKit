updating
subscribe
	super subscribe.
	(self announcer)
		observe: OBSelectionChanged
		send: #columnSelected:
		to: OBRecentClasses default