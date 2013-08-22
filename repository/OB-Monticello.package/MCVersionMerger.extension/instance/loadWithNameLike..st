*ob-monticello
loadWithNameLike: aString
	aString ifNil: [merger load] ifNotNil: [merger loadWithNameLike: aString].
	records do: [:ea | ea updateWorkingCopy]