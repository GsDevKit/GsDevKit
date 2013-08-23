updating
subscribe
	(self announcer)
		observe: OBSelectionChanged
			send: #relabel:
			to: self;
		observe: OBNodeCommandScan
			send: #scanNodeCommands:
			to: self;
		observe: OBTextCommandScan
			send: #scanTextCommands:
			to: self