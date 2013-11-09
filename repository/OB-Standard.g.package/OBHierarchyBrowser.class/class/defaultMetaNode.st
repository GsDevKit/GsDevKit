configuration
defaultMetaNode
	^ self 
		buildMetagraphOn: (OBMetaNode named: 'RootClass')
		class: #classHierarchy 
		comment: #commentHierarchy 
		metaclass: #metaclassHierarchy.
