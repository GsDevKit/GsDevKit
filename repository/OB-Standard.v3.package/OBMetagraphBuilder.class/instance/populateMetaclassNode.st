private
populateMetaclassNode
	(self metaclassMetaNode)
		ancestrySelector: #isDescendantOfClass:;
		childAt: #allCategory put: self allMethodCategoryMetaNode;
		childAt: #categories put: self methodCategoryMetaNode;
		autoSelect: self allMethodCategoryMetaNode