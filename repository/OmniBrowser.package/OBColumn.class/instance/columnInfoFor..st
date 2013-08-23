callbacks
columnInfoFor: aJadeServer

	| l icons allNil |
	l := self list.
	icons := OrderedCollection new.
	allNil := true.
	1 to: l size do: [:i | | icon |
		icon := self iconAt: i.
		icon ~~ nil ifTrue: [ allNil := false ].
		icons add: icon asString].
	allNil ifTrue: [icons := #()].
	^(l select: [:ea | ea isString not ]) isEmpty
		ifTrue: [ aJadeServer stringFromCollection: (Array with: self selection printString with: l size printString with: icons size printString ), l, (icons asArray)]
		ifFalse: [ (Array with: self selection with: l size with: icons size ), l, (icons asArray) ]