testing
testIssue253
	"unsubscribe during announcement ... hard to duplicate problem since we're using an IdentityDictionary 
	 and collistionbucket density is a function of oop density"
	
	| block announcement |
	block := [ :ann | announcer unsubscribe: block ].
	announcer
		subscribe: AnnouncementMockA
			send: #value:
			to: block;
		subscribe: AnnouncementMockB
			send: #value:
			to: block;
		subscribe: AnnouncementMockC
			send: #value:
			to: block;
		subscribe: AnnouncementMockD
			send: #value:
			to: block;
		yourself.
		
	announcer announce: AnnouncementMockA.
