testing
testUnsubscribeBlock
	| announcement |
	announcer
		subscribe: AnnouncementMockA
		do: [ :ann | announcement := ann ]
		for: self.
	announcer
		unsubscribe: self.

	announcement := nil.
	announcer announce: AnnouncementMockA new.
	self assert: announcement isNil