*ob-gemstone-platform-GSExtensions
sortNameMatchesAsString: matches
	| sorted |
	sorted := matches asSortedCollection: [ :match1 :match2 | match1 moreRelevantThan: match2].
	sorted := sorted size > 80 
		ifTrue: [(sorted copyFrom: 1 to: 80) add: (NameMatch new name: #'***results_truncated***' matching:  ''); yourself ]
		ifFalse: [ sorted ].
	^sorted inject: String new into: [ :result :match | result, match name, ' ' ]