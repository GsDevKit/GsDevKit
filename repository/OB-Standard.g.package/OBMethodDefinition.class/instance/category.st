accessing
category
	category == nil
		ifTrue: [ 
			category := self selector == nil 
				ifTrue: [ ClassOrganizer default  ]
				ifFalse: [ self theClass whichCategoryIncludesSelector: self selector]].
	^category