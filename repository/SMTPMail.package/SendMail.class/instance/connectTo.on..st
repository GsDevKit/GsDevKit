private
connectTo: portNumber on: host 
	"Connect the receiver to the server socket identified by portNumber and 
	host. host can be the name of the host or its address. 
	Returns true if the connection succeeded and false if not."

	| res |
	res := true.
	[res isKindOf: Boolean]
		whileTrue: 
			[res := self
						_twoArgPrim: 2
						with: host
						with: portNumber.
			res == false
				ifTrue: 
					["would have blocked"
					(self writeWillNotBlockWithin: 100)
						ifTrue: [ res := self ]
						ifFalse: 
							[ res := nil ]]].
	^res == nil
		ifTrue: [false]
		ifFalse: [true]
