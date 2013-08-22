displaying
name
	| stamp |
	stamp := version stamp ifNil: ['<timestamp missing>'].
	^ version theClassName, '>>', version selector, ' ', stamp