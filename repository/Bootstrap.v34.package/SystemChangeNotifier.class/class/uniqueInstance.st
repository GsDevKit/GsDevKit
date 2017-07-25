accessing
uniqueInstance	
	| ui |
	ui := SessionTemps current at: #SystemChangeNotifier_UniqueInstance otherwise: nil.
	ui == nil 
		ifTrue: [
			ui := self new.
			SessionTemps current at: #SystemChangeNotifier_UniqueInstance put: ui].
	^ui