testing
testSubscribeSend
	| announcement |
	announcer
		subscribe: AnnouncementMockA
		send: #value:
		to: [ :ann | announcement := ann ].
		
	announcement := nil.
	announcer announce: AnnouncementMockA.
	self assert: (announcement isKindOf: AnnouncementMockA).
	
	announcement := nil.
	announcer announce: AnnouncementMockB new.
	self assert: announcement isNil