private
buildClassNodeIn: nodeDictionary 
	(nodeDictionary at: #class)
		childAt: #instanceVariables
			labeled: 'instance'
			put: (nodeDictionary at: #var);
		childAt: #classVariables
			labeled: 'class'
			put: (nodeDictionary at: #var);
		addFilter: OBModalFilter new