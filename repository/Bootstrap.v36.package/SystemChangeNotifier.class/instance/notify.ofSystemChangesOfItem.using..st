public
notify: anObject ofSystemChangesOfItem: itemKind  using: oneArgumentSelector 

	| announcementClass |
	announcementClass := (itemKind == #method)
					ifTrue: [ MethodChangeAnnouncement ]
					ifFalse: [ ClassChangeAnnouncement ].
	self
		observe: announcementClass
		send: oneArgumentSelector
		to: anObject