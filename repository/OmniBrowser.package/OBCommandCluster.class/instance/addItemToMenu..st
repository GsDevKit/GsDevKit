accessing
addItemToMenu: aMenu
	| submenu |
	submenu _ aMenu addSubmenu: label enabled: self isEnabled.
	self populateMenu: submenu