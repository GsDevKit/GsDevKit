testing
isAbstractTestCase

	self coParents do: [:ea | ea theClass isAbstract ifFalse: [ ^false ]].
	^true