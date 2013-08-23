user interface
addMoreToMenu: aMenu
	aMenu addLine.
	aMenu
		add: 'more...' translated
		action: #shiftedTextPaneMenuRequest