parsing
parseDocument
	[self driver nextEntity isNil or: [self eod]] whileFalse