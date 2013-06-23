entities
initEntities
	| ents |
	ents _ Dictionary new.
	ents
		at: 'amp' put: (DTDEntityDeclaration name: 'amp' value: $&);
		at: 'quot' put: (DTDEntityDeclaration name: 'quot' value: $");
		at: 'apos' put: (DTDEntityDeclaration name: 'apos' value: $');
		at: 'gt' put: (DTDEntityDeclaration name: 'gt' value: $>);
		at: 'lt' put: (DTDEntityDeclaration name: 'lt' value: $<).
	^ents