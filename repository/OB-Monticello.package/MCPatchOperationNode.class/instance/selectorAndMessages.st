as yet unclassified
selectorAndMessages

	| def |
	def := self operation definition.
	^{OBMessageNode 
		on: def selector 
		inMethod: def selector 
		inClass: def actualClass }