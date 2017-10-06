running
execute
	[ [ self model merge ]
		on: MCMergeResolutionRequest
		do: [ :request |
			request merger conflicts isEmpty
				ifTrue: [ request resume: true ]
				ifFalse: [ request pass ] ] ]
		valueSupplyingAnswers: #(('No Changes' true)).
	self gofer cleanup