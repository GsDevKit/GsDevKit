updating
subscribe
	(self announcer)
		observe: OBSelectionChanged
			send: #updateButtons:
			to: self;
		observe: OBNodeCommandScan
			send: #scanNodeCommands:
			to: self;
		observe: OBTextCommandScan
			send: #scanTextCommands:
			to: self