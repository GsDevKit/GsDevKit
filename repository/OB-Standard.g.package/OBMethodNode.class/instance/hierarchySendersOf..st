private
hierarchySendersOf: aSelector
	| hierarchySenders |
	hierarchySenders := (SystemNavigation default allCallsOn: aSelector) select: [:ea | (self theClass withAllSuperclasses includes: ea actualClass) or: [
			                                       self theClass theNonMetaClass allSubclasses includes: ea actualClass]].
	^hierarchySenders asSortedArray collect: [:ref | OBMessageNode on: aSelector inMethodReference: ref]