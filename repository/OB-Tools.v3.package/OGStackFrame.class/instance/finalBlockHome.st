accessing
finalBlockHome

	(self receiver isKindOf: ExecutableBlock) ifFalse: [ ^self ].
	^self sender finalBlockHomeFor: self