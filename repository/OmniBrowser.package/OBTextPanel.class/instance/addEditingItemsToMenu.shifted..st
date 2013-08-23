user interface
addEditingItemsToMenu: aMenu shifted: aBoolean
	| items |
	items _  aBoolean 
		ifTrue: [self shiftedYellowButtonMenu] 
		ifFalse: [self yellowButtonMenu].
	items do: [:ea |
			  ea = #-
				ifFalse: [aMenu add: ea first action: ea second]
				ifTrue: [aMenu addLine]].