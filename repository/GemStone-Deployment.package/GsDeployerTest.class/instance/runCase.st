running
runCase
	(self doSilently) ifFalse: [ ^super runCase ].
	^ SystemChangeNotifier uniqueInstance doSilently: [ super runCase ]