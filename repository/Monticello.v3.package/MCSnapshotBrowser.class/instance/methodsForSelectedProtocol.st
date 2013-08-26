message category list
methodsForSelectedProtocol
	| methods |
	(protocolSelection := self messageCategoryListSelection)
		ifNil: [^ Array new].
	methods := self methodsForSelectedClass asOrderedCollection.
	methods removeAllSuchThat: [:ea | ea category ~= protocolSelection].
	^ methods
