private
connectNodesIn: nodeDictionary 
	self
		buildClassNodeIn: nodeDictionary;
		buildVarNodeIn: nodeDictionary;
		buildMethodNodeIn: nodeDictionary