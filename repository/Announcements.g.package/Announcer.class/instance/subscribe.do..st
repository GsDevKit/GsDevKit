subscription
subscribe: anAnnouncementClass do: aValuable 
	"GemStone cannot get the receiver from a BlockClosure, so use subscribe:do:for:"
	
	^self subscribe: anAnnouncementClass do: aValuable for: aValuable receiver