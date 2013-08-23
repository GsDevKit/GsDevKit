execution
execute
	|viewType|
	self flag: #portToGemStone.
	viewType := OBChoiceRequest labels: #(Source Bytecode Decompile PrettyPrint).
	viewType ifNotNil: [self perform: (#toggle, viewType) asSymbol]