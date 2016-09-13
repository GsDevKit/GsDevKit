subscription
observe: aClass do: aValuable 
	| actions |
	aClass == nil ifTrue: [ ^self ].
	actions _ self subscriptions at: aClass ifAbsent: [ActionSequence new].
	self subscriptions at: aClass put: (actions copyWith: aValuable).