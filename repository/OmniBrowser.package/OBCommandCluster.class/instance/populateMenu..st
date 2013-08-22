accessing
populateMenu: aMenu 
	self groupedCommands	
		do: [:group | group do: [:cmd | cmd addItemToMenu: aMenu]]
		separatedBy: [aMenu addLine]