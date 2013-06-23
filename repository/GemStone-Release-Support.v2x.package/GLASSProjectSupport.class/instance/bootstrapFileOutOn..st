bootstrap fileout
bootstrapFileOutOn: stream
	| organizer dict file |
	organizer := ClassOrganizer new.

	(Class selectorsIn: '*core-squeak-subclass creation') do: [:sel |
		stream 
			nextPutAll: 'category: ''*core-squeak-subclass creation'''; 
			nextPut: Character lf.
		Class fileOutMethod: sel on: stream ].
	
	dict := SymbolDictionary new.
	dict at: #DiskProxy put: DiskProxy.
	(organizer determineClassFileoutOrder: dict) do: [:cl |
		self fileOutClass: cl on: stream ].

	dict := SymbolDictionary new.
	{ 'Core-SharedPool'. 'Core-Gemstone'. } do: [:category |
		(SystemOrganizer new listAtCategoryNamed: category) do: [:sym |
			dict at: sym put: (Smalltalk at: sym) ]].
	self fileOutClasses: dict on: stream organizer: organizer.

	(PackageInfo named: 'Core-Squeak') extensionMethods do: [:methodRef |
		stream 
			nextPutAll: 'category: '''; nextPutAll: (methodRef actualClass _categoryOfSelector: methodRef methodSymbol);
      		nextPut: $'; nextPut:  Character lf.
		methodRef actualClass fileOutMethod: methodRef methodSymbol on: stream ].

	dict := SymbolDictionary new.
	{ 'Core-Squeak'. 'Core-MCInstaller'. 'Core-Bootstrap'. } do: [:category |
		(SystemOrganizer new listAtCategoryNamed: category) do: [:sym |
			dict at: sym put: (Smalltalk at: sym) ]].
	self fileOutClasses: dict on: stream organizer: organizer.

	self coreInitializationOn: stream