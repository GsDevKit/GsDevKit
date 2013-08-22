selecting
fan: aFan selection: index
	self listHolder fan: aFan selection: index.
	selection := index.
	self selectedNode ifNotNilDo: [:node | self listHolder noteChild: node].
	self switchFilter: self listHolder switchFilter.
	switch refresh.
	self changed: #list