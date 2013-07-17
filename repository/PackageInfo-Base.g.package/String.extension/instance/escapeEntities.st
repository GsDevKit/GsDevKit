*packageinfo-base
escapeEntities
	^ String streamContents: [:s | self do: [:c | s nextPutAll: c escapeEntities]]