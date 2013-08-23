navigating
subscribe
	super subscribe.
	self announcer
		observe: OBShowSenders send: #showSenders: to: self;
		observe: OBShowImplementors send: #showImplementors: to: self.