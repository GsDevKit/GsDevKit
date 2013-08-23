comparing
= other 
	^ (other isKindOf: self class) and: [ name = other theNonMetaName and: [ isMeta = other isMeta ] ]