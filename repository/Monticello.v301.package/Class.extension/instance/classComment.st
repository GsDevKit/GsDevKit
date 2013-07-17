*monticello
classComment

	| desc comment |
	(desc := self _description) ifNil:[ 
		(self class includesSelector: #comment) 
			ifTrue: [ ^self comment].
			^ '' ].
	(comment := desc detailsAboutClass) ifNil: [ ^'' ].
	^comment