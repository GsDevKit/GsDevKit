subscription
unsubscribe: anObject
	subscriptions keysAndValuesDo:
		[:class :actions |
		subscriptions at: class put: (actions reject: [:ea | ea receiver == anObject])].
	subscriptions keysAndValuesRemove: [:key :value | value isEmpty]