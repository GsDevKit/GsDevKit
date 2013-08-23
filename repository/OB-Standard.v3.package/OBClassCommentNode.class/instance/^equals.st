comparing
= aClassOrCommentNode
	^(aClassOrCommentNode isKindOf: OBClassAwareNode) and: [self theClass = aClassOrCommentNode theClass]