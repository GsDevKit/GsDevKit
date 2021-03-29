accessing
shouldPersistHistory

	^SessionTemps current at: #MethodVersionHistory_ShouldPersistHistory otherwise: true.