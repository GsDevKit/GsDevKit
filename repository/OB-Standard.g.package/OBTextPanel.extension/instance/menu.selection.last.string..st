*ob-standard
menu: shifted selection: first last: last string: aString

	| menu  |
	menu := OGMenu new.
	self ogMenu: menu shifted: shifted selection: (OBTextSelection on: (first to: last) inText: aString).
	^menu