private
buildVarNodeIn: nodeDictionary 
	(nodeDictionary at: #var)
		childAt: #accessors put: (nodeDictionary at: #method);
		addFilter: OBClassSortFilter new