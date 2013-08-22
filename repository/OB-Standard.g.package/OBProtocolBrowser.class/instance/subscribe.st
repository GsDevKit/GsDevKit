updating
subscribe
	super subscribe.
	self announcer
		observe: OBRefreshRequired  
		send:  #refresh:
		to: self;
		observe: OBNodeChanged 
		send:  #limitClass: 
		to: self.