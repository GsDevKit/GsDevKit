user interface
addItemToMenu: aMenu
	self isActive ifTrue:
		[aMenu
			add: self labelWithKeystroke
			target: self 
			selector: #execute
			enabled: self isEnabled
			icon: self icon]