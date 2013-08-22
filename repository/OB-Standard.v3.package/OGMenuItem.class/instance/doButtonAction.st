actions
doButtonAction
	"Called programattically, this should trigger the action for which the receiver is programmed"

	self isEnabled ifFalse: [^ self].
	selector ifNil:[^self].
	target perform: selector