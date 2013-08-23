filtering
nodesFrom: aCollection forNode: aNode
	| pat |
	(pat := self pattern) == nil ifTrue: [ ^aCollection ].
	^ aCollection select: [ :each | pat match: each name ]