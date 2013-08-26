accessing
listAtCategoryNamed: cat

	^((self categoryDict at: cat asString otherwise: #()) select: [:each |(System myUserProfile symbolList objectNamed: each name asString) ~~ nil]) collect: [:ea | ea name ]