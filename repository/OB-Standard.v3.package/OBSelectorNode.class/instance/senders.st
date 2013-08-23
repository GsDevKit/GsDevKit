navigating
senders
	^ (SystemNavigation default allCallsOn: self selector) asSortedArray
			collect: [:ref | OBMessageNode on: self selector inMethodReference: ref]