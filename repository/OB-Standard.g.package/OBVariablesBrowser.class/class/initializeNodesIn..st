private
initializeNodesIn: nodeDictionary 
	nodeDictionary
		at: #class put: (OBMetaNode named: 'Class');
		at: #var put: (OBMetaNode named: 'Variable');
		at: #method put: (OBMetaNode named: 'Method');
		at: #message put: (OBMetaNode named: 'Message')