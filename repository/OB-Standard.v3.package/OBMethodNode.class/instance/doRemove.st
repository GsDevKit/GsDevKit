private
doRemove
	self theClass removeSelector: self selector.
	self signalDeletion