*ob-standard
fileOutCategory: cat on: aStream
	"Store on aStream all the classes associated with the category."

	| classes co symDict order |
	classes := (self listAtCategoryNamed: cat) collect: [:ea | Smalltalk classNamed: ea ].
	co := ClassOrganizer defaultList: classes.
	symDict := SymbolDictionary new.
	classes do: [:cl | symDict at: cl name put: cl ].
	order := co determineClassFileoutOrder: symDict.
	self fileOutClasses: order on: aStream.
	co fileOutMethods: symDict order: order on: aStream