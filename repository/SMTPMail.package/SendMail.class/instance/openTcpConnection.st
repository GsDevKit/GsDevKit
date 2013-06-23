private
openTcpConnection
	| ok |
	ok := self connectTo: self smtpPort on: self mailhostName.
	ok ifFalse: [self close].
	^ok
