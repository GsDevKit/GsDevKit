subscription
announce: anObject

	| ann |
	ann := anObject asAnnouncement.
	subscriptions keysAndValuesDo:
		[:class :action |
		(ann isKindOf: class) ifTrue: [action valueWithArguments: (Array with: ann)]].
	^ ann