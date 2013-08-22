instance creation
onSymbolList: aSymbolList symbolDictionary: aSymbol
	^ self selection: (OBSymbolDictionaryNode 
						on: aSymbol 
						inSymbolList: aSymbolList)