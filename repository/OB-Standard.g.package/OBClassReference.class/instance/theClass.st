accessing
theClass
	| theClass |
	theClass := self theNonMetaClass ifNil: [^ nil].
	^ isMeta
		ifFalse: [theClass]
		ifTrue: [theClass class]