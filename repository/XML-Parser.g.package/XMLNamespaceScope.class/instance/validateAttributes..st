validation
validateAttributes: attributeList
	"check all attribute namespaces are defined and not duplicated by aliasing"
	| namespace localName |
	attributeList keysDo: [:attrName |
		self splitName: attrName into: [:ns :ln |
			namespace _ ns.
			localName _ ln].
		namespace ifNotNil: [
			(self namespaceAliases: namespace) do: [:alias |
				(attributeList includesKey: alias , ':' , localName)
					ifTrue: [self parseError: 'Attributes ' , attrName , ' and ' , alias , ':' , localName , ' are aliased to namespace ' , (self namespaceURIOf: namespace) ]]]]