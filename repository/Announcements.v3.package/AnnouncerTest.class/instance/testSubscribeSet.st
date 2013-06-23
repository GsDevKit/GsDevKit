testing
testSubscribeSet
	| announcement |
	announcer
		subscribe: AnnouncementMockA , AnnouncementMockB
		do: [ :ann | announcement := ann ]
		for: self.
		
	announcement := nil.
	announcer announce: AnnouncementMockA.
	self assert: (announcement isKindOf: AnnouncementMockA).
	
	announcement := nil.
	announcer announce: AnnouncementMockB new.
	self assert: (announcement isKindOf: AnnouncementMockB).