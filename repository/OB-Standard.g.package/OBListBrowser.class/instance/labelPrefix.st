accessing
labelPrefix
	^ labelPrefix ifNil: [labelPrefix := self root metaNode edges first label capitalized, ' of']