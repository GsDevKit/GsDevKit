private
populateMethodCategoryNode
	self methodCategoryMetaNode 
		ancestrySelector: #isDescendantOfMethodCat:;
		childAt: #methods put: self methodMetaNode