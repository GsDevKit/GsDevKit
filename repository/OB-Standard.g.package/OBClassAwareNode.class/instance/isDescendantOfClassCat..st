ancestry
isDescendantOfClassCat: aClassCategoryNode
	"fast for GemStone"
	
	^ aClassCategoryNode name asString = self theClass category asString