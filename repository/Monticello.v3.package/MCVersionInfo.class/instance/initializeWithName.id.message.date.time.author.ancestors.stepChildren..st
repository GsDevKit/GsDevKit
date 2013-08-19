initialize-release
initializeWithName: vName id: aUUID message: aString date: aDate time: aTime author: initials ancestors: aCollection stepChildren: stepCollection
	name := vName asString asMCVersionName.
	id := aUUID.
	aString ~~ nil
		ifTrue: [ message := aString withSqueakLineEndings ].
	date := aDate.
	time := aTime.
	author := initials.
	ancestors :=  aCollection.
	stepChildren := stepCollection