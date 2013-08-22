ancestry
isDescendantOfMethodCat: aMethodCatNode
	^ (self theClass categoryOfSelector: self selector) = aMethodCatNode name