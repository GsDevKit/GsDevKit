private
sendersOf: aSelector
	^ (SystemNavigation default allCallsOn: aSelector) asSortedArray
			collect: [:ref | OBMessageNode on: aSelector inMethodReference: ref]