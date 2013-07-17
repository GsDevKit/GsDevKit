*packageinfo-base
untilEnd: aBlock displayingProgress: aString
	[self atEnd] whileFalse:
		[aBlock value]