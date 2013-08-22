*ob-standard
ogMenu: aMenu shifted: aBoolean selection: aTextSelection 
	"The shifted menu items all operate within the context of Squeak, which is not
	 useful for GemStone"
	
	| items |
	items _  self yellowButtonMenu.
	items do: [:ea |
			  ea = #-
				ifFalse: [aMenu add: ea first action: ea second]
				ifTrue: [aMenu addLine]].
	self addTextCommandsToMenu: aMenu selection: aTextSelection.
	^ aMenu