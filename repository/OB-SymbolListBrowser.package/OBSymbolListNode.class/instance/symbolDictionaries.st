navigating
symbolDictionaries
	^ symbolList collect: [:symDict | OBSymbolDictionaryNode 
															on: symDict name 
															inSymbolList: symbolList]