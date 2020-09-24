private
objectQueue

	ObjectQueue == nil ifTrue: [ ObjectQueue := RcQueue new: 100 ].
	^ObjectQueue