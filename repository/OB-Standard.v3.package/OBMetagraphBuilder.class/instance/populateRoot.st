private
populateRoot
	self root
		childAt: self classSelector
			labeled: 'instance'
			put: self classMetaNode;
		childAt: self commentSelector
			labeled: '?'
			put: self classCommentMetaNode;
		childAt: self metaclassSelector
			labeled: 'class'
			put: self metaclassMetaNode;
		addFilter: OBModalFilter new;
		addFilter: OBClassSortFilter new.