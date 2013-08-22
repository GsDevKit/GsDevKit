execution
execute
	"create accessors which are currently not defined"
	| theClass newLine varName | 
	newLine:= Character lf asString.
	theClass := target theNonMetaClass.
	varName := 'newValue'.
	
	theClass instVarNames do: [:aSymbol | | aSelector |
		aSelector := aSymbol asString.
		(theClass includesSelector: aSymbol) ifFalse: [
			(theClass compileMethod: (aSelector, newLine , newLine ,
			'   "Return the value of the instance variable ''' , aSelector ,
			'''."' , newLine , '   ^' , aSelector , newLine)
			dictionaries:  #() 
			category: #accessing)].
		(theClass includesSelector: (aSelector,':') asSymbol) ifFalse: [
			(theClass compileMethod: (aSelector , ': ' , varName , newLine , newLine ,
				'   "Modify the value of the instance variable ''' , aSelector ,
				'''."' , newLine , '   ' , aSelector , ' := ' , varName , newLine)
				dictionaries:  #() 
				category: #updating) ] ].
		
	self doAutoCommit