public
childAncestorOf: aNode indexIn: aBlock 
	^ self metaNode 
		childOf: self
		ancestorOf: aNode
		indexIn: aBlock