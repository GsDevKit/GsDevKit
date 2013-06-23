private
announcementClassForItem: itemKind change: changeKind

	| className |
	className := (itemKind == #method)
					ifTrue: ['Method']
					ifFalse: ['Class'].
	changeKind == #Added
		ifTrue: [ className := className, 'Added' ].
	changeKind == #Commented
		ifTrue: [ className := className, 'Commented' ].
	changeKind == #Modified
		ifTrue: [ className := className, 'Modified' ].
	changeKind == #Renamed
		ifTrue: [ className := className, 'Modified' ].
	changeKind == #Removed
		ifTrue: [ className := className, 'Removed' ].
	className := className, 'Announcement'.
	^System myUserProfile objectNamed: className asSymbol
	