handling tokens
handleEndTag: elementName
	| namespace localName namespaceURI qualifiedName |

	self usesNamespaces
		ifTrue: [
			self splitName: elementName into: [:ns :ln |
				namespace _ ns.
				localName _ ln].

			"ensure our namespace is defined"
			namespace
				ifNil: [
					namespace := self scope defaultNamespace.
					qualifiedName := namespace , ':' , elementName]
				ifNotNil: [
					namespaceURI := self scope namespaceURIOf: namespace.
					namespaceURI
						ifNil: [self parseError: 'Start tag ' , elementName , ' refers to undefined namespace ' , namespace asString].
					qualifiedName := elementName].

			"call the handler"
			self saxHandler
				checkEOD; 
				endElement: localName namespace: namespace namespaceURI: namespaceURI qualifiedName: qualifiedName.
			self scope leaveScope]
		ifFalse: [
			"call the handler"
			self saxHandler
				checkEOD; 
				endElement: elementName namespace: nil namespaceURI: nil qualifiedName: elementName]