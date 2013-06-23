initialize-release
initialize
	| mailType |
	super initialize.
	self mailhostName: self class mailhost.
	self from: self class mailfrom
