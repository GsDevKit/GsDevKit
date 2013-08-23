private
stringFromCollection: aCollection filter: filter

	| stream tab expanded |
	stream := WriteStream on: String new.
	tab := String with: Character tab.
	expanded := ' '.
	aCollection do: [:each | | res |
		res := (filter and: [each isString and: [each includes: Character tab ]])
			ifTrue: [ each copyReplaceAll: tab with: expanded ]
			ifFalse:  [ each ].
		stream nextPutAll: res ; tab.
	].
	^stream contents.