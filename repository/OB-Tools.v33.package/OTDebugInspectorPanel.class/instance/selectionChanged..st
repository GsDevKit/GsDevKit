events
selectionChanged: anAnnouncement
	| node |
	node := anAnnouncement node ifNil: [ ^ self ].
	self inspector: receiverInspector display: node doItReceiver.
	self inspector: contextInspector display: node doItContext