as yet unclassified
extractInfoFrom: dict
	dict at: #id put: (UUID fromString: (dict at: #id)).
	dict at: #date ifPresent: [:dateString | 
		dict at: #date put: (
		[ | d |
			[d := Date fromString: dateString] on: Error do: [ :ex | ex return: nil ].
			d == nil ifTrue: [ d := Date fromString: dateString usingFormat: #(2 1 3 $/ 1 2 $: false ) ].
			d ] on: Error do: [ :ex | ex return: nil ])].
	dict at: #time ifPresent: [:t | t isEmpty ifFalse: [dict at: #time put: (Time readFrom: t readStream)]].
	dict at: #ancestors ifPresent: [:a | dict at: #ancestors put: (a collect: [:ea | self extractInfoFrom: ea])].
	^ dict