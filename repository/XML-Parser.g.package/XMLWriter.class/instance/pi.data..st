writing xml
pi: piTarget data: piData
	self startPI: piTarget.
	self stream nextPutAll: piData.
	self endPI