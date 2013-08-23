callbacks
menu: aMenu shifted: aBoolean selection: aTextSelection 
	self addEditingItemsToMenu: aMenu shifted: aBoolean.
	self addTextCommandsToMenu: aMenu selection: aTextSelection.
	self addMoreToMenu: aMenu.
	^ aMenu