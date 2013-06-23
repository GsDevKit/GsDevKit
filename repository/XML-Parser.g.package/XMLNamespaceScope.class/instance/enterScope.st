scope
enterScope
	self scope addLast: (Array with: self defaultNamespace with: nil with:currentBindings)