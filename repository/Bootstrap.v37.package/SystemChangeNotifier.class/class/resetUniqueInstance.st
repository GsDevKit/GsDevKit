accessing
resetUniqueInstance
	"self resetUniqueInstance"

	SessionTemps current at: #SystemChangeNotifier_UniqueInstance put: nil.