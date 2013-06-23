accessing
objectLog
	"expect the caller to abort, acquire lock, and commit if necessary"

	ObjectLog == nil ifTrue: [ ObjectLog := OrderedCollection new ].
	ObjectLog addAll: (self objectQueue _timeSortedComponents collect: [:ea | ea value]).
	self objectQueue removeAll.
	^ObjectLog