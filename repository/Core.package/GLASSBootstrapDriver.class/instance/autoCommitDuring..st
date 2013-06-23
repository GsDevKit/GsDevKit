auto commit
autoCommitDuring: aBlock

	| commitThreshold |
	commitThreshold :=  75.
	"Install AlmostOutOfMemory handler"
	Exception
		category: GemStoneError
		number: (ErrorSymbols at: #rtErrSignalAlmostOutOfMemory)
		do: [:ex :cat :num :args |
			"Exception caught, do a commit."
			System commitTransaction 
				ifFalse: [ self error: 'AutoCommit failed' ].
			"run a markSweep"
			System _vmMarkSweep.
			(System _tempObjSpacePercentUsedLastMark < commitThreshold)
				ifTrue: [
					"We dropped below the threshold reenable the signal"
					System enableAlmostOutOfMemoryError ].
			"continue execution" ].
	"Enable AlmostOutOfMemory signal"
	System signalAlmostOutOfMemoryThreshold: commitThreshold.
	aBlock ensure: [
		"disable AlmostOutOfMemory signal"
		System signalAlmostOutOfMemoryThreshold: -1].