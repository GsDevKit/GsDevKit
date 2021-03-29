comparing
= other

	self species = other species ifFalse: [ ^false ].
	self behavior = other behavior ifFalse: [ ^false ].
	self selector = other selector ifFalse: [ ^false ].
	self category = other category ifFalse: [ ^false ].
	self changeStamp = other changeStamp ifFalse: [ ^false ].
	self source = other source ifFalse: [ ^false ].
	^true