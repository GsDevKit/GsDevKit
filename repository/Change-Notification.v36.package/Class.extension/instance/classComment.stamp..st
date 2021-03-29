*change-notification
classComment: comment stamp: aString

self comment: comment.
self _extraDictRemoveKey: #description. "Bug #42589"
aString == nil
	ifTrue: [ self commentStamp: self changeStamp ]
	ifFalse: [ self commentStamp: aString ].
SystemChangeNotifier uniqueInstance
	announce: (ClassCommentedAnnouncement itemClass: self)