handling tokens
handleStartTag: elementName attributes: attributeList namespaces: namespaces

	| localName namespace namespaceURI |

	(attributeList includesKey: 'xml:lang')
		ifTrue: ["languageEnvironment := LanguageEnvironment localeID: (LocaleID isoString: (attributeList at: 'xml:lang'))"].
	self usesNamespaces
		ifTrue: [
			self scope enterScope.
				"declare any namespaces"	
				namespaces keysAndValuesDo: [:ns :uri |
					self scope declareNamespace: ns uri: uri].

			self splitName: elementName into: [:ns :ln |
				namespace _ ns.
				localName _ ln].

			"ensure our namespace is defined"
			namespace
				ifNil: [namespace := self scope defaultNamespace]
				ifNotNil: [
					namespaceURI := self scope namespaceURIOf: namespace.
					namespaceURI
						ifNil: [self parseError: 'Start tag ' , elementName , ' refers to undefined namespace ' , namespace asString]].

			self validatesAttributes
				ifTrue: [self scope validateAttributes: attributeList].
			"call the handler"
			self saxHandler
				checkEOD; 
				startElement: localName namespaceURI: namespaceURI namespace: namespace attributeList: attributeList]
		ifFalse: [
			"call the handler"
			self saxHandler
				checkEOD; 
				startElement: elementName namespaceURI: nil namespace: nil attributeList: attributeList]