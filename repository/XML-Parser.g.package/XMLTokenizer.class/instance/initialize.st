initialize
initialize
	parsingMarkup _ false.
	validating _ false.
	attributeBuffer _ WriteStream on: (String new).
	nameBuffer _ WriteStream on: (String new)