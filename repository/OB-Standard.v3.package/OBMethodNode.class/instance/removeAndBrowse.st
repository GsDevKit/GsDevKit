private
removeAndBrowse
	self simpleBrowseSenders.
	self theClass removeSelector: self selector.
	self signalDeletion