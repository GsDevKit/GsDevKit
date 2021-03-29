accessing
_objectLog
	"direct access to ObjectLog - should have acquired ObjectLogLock if removing entries from ObjectLog"

	ObjectLog == nil ifTrue: [ ObjectLog := OrderedCollection new ].
	^ObjectLog