subscription
observe: aClass do: aValuable 
	| actions |
	actions _ subscriptions at: aClass ifAbsent: [ActionSequence new].
	subscriptions at: aClass put: (actions copyWith: aValuable).