subscription
subscribe: anAnnouncementClass send: aSelector to: anObject
	^ self subscribe: anAnnouncementClass do: (MessageSend receiver: anObject selector: aSelector)