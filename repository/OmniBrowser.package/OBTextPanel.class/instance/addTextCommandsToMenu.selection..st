callbacks
addTextCommandsToMenu: aMenu selection: aTextSelection
	| scan |
	scan _ self announce: OBTextCommandScan.
	scan
		populateMenu: aMenu
		withNodes: {aTextSelection}
		forRequestor: self.