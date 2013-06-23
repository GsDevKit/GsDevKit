public
noMoreNotificationsFor: anObject

	subscriptions keysAndValuesDo: [ :class :actions |
		subscriptions at: class put: (actions 
			reject: [ :each | each receiver == anObject ]) ].
	subscriptions keysAndValuesRemove: [ :class :actions |
		actions isEmpty ]