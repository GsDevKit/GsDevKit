updating
announce: anObject
	^ OBAnnouncer current ifNotNilDo: [:announcer | announcer announce: anObject]