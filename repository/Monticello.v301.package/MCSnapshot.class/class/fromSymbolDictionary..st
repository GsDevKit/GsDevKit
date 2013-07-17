as yet unclassified
fromSymbolDictionary: aSymbolDictionary
	"
	MCCodeTool openRoot: 
		(MCBrowseNode new 
			patch: ((MCSnapshot fromSymbolDictionary: SessionMethods)
						patchRelativeToBase: MCSnapshot empty); 
			label: 'SessionMethods'; 
			yourself).
	"
	| definitions gsPackage |
	definitions := OrderedCollection new.
	(aSymbolDictionary values select: [:global | global isBehavior ]) do: [:class | 
		definitions add: class asClassDefinition.
		(class persistentMethodDictForEnv: 0) keysAndValuesDo: [:selector :gsmethod |
			definitions add: ((MethodReference new
						setClassSymbol: class name
						classIsMeta: class isMeta
						methodSymbol: selector
						stringVersion: '')
							symbolDictionary: aSymbolDictionary;
							yourself) asMethodDefinition ]].
	(gsPackage := aSymbolDictionary at: GsPackage globalName ifAbsent: []) notNil
		ifTrue:  [
			gsPackage behaviorAndMethodDictDo: [:class :methodDict |
				methodDict keysAndValuesDo: [:selector :gsmethod |
				definitions add: ((MethodReference new
					setClassSymbol: class theNonMetaClass name
					classIsMeta: class isMeta
					methodSymbol: selector
					stringVersion: '')
						symbolDictionary: ((System myUserProfile dictionaryAndSymbolOf: class theNonMetaClass) at: 1);
						yourself) asMethodDefinition ]]].
	^self fromDefinitions: definitions