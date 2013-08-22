navigating
sendersOfMessage
	^ (SystemNavigation default allCallsOn: self selector) asSortedArray
			collect: [:ref | OBMessageNode on: self selector inMethodReference: ref]