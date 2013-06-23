announce
announce: anAnnouncement
	| announcement actionList |
	announcement := anAnnouncement asAnnouncement.
	actionList := OrderedCollection new.
	subscriptions keysAndValuesDo: [ :class :actions |
		(class handles: announcement) 
			ifTrue: [ actionList add: {actions. announcement} ]].
	actionList do: [:array |  (array at: 1) valueWithArguments: (Array with: (array at: 2)) ].
	^ announcement