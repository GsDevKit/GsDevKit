execution
pickRepository 
	| repos labels |
	repos := MCRepositoryGroup default repositories.
	labels := repos collect: [:ea | ea description].
	^OBChoiceRequest 
			prompt: 'Copy to:' 
			labels: labels
			values: repos