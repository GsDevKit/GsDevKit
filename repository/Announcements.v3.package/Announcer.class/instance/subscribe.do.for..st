subscription
subscribe: anAnnouncementClass do: aValuable for: anObject
	| valuable actions |
	valuable := ((aValuable respondsTo: #receiver) and:[anObject == aValuable receiver])
		ifTrue: [ aValuable ]
		ifFalse: [ AnnouncerSubscription valuable: aValuable subscriber: anObject ].
	actions := subscriptions at: anAnnouncementClass ifAbsent: [ ActionSequence new ].
	subscriptions at: anAnnouncementClass put: (actions copyWith: valuable).
	^ aValuable