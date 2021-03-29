announcements
announce: anObject

	| ann |
	ann _ anObject asAnnouncement.
	self silenceLevel > 0 ifTrue: [ ^nil ].
	self subscriptions keysAndValuesDo:
		[:class :action |
		(ann isKindOf: class) ifTrue: [action valueWithArguments: (Array with: ann)]].
	^ann