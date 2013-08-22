configuration
defaultMetaNode
	| env classCat |
	env := OBMetaNode named: 'Environment'.
	classCat := OBMetaNode named: 'ClassCategory'.
	env childAt: #categories put: classCat.
	classCat ancestrySelector: #isDescendantOfClassCat:.
	self buildMetagraphOn: classCat.
	^env