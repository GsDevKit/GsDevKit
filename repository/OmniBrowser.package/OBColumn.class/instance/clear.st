updating
clear
	self switchFilter: nil.
	self listHolder clearAll.
	self changed: #list.
	self changed: #selection