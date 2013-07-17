*change-notification
classComment: comment stamp: aString

| doc txt |
doc := GsClassDocumentation newForClass: self.
txt := (GsDocText new) details: comment.
doc documentClassWith: txt.
self _description: doc.
aString == nil
	ifTrue: [ self commentStamp: self changeStamp ]
	ifFalse: [ self commentStamp: aString ].
SystemChangeNotifier uniqueInstance
	announce: (ClassCommentedAnnouncement itemClass: self)