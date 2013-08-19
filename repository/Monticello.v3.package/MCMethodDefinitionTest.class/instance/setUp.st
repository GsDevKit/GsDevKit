running
setUp
	super setUp.
	navigation := (Smalltalk hasClassNamed: #SystemNavigation)
		ifTrue: [(Smalltalk at: #SystemNavigation) new]
		ifFalse: [Smalltalk].
	isModified := self ownPackage modified.
	overrideTimestamp := (MethodReference class: self class selector: #override) timeStamp