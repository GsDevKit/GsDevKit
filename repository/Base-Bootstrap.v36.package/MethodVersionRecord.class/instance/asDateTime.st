converting
asDateTime
	"use DateTime from 1970 if we dont find any useful format"
	| anArray |
	anArray := changeStamp subStrings: $ .
	^(anArray size == 3 and: [(anArray at: 2) size = 10])
		ifTrue: [DateTime fromString: (anArray at: 2), ' ' ,(anArray at: 3) usingFormat: #(2 1 3 $/ 1 1 $: true false false)]
		ifFalse: [DateTime newWithYear: 1970 dayOfYear: 1 seconds: 0]