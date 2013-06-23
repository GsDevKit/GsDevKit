private
sendRcptTo
	| result adresses |
	result := true.
	"check if explicitely set"
	adresses := (self rcptTo == nil)
		ifTrue: [(self to subStrings: $;) ,(self cc subStrings: $;)]
		ifFalse: [self rcptTo].
	adresses do: [:mailAddress |
			mailAddress isEmpty
				ifFalse: [result := result and: [self sendCrlfString: ('RCPT TO:<', mailAddress add: '>'; yourself)] ] ].
	^result
