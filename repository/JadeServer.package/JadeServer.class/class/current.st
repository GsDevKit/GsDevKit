accessing
current

	| cur |
	cur := SessionTemps current at: #JADESERVER_CURRENT otherwise: nil.
	cur == nil
		ifTrue: [
			cur := self new.
			SessionTemps current at: #JADESERVER_CURRENT put: cur].
	^cur