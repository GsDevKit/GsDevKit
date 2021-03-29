public
notify: anObject ofSystemChangesOfItem: itemKind change: changeKind using: oneArgumentSelector 

	self
		observe: (self announcementClassForItem: itemKind change: changeKind)
		send: oneArgumentSelector
		to: anObject