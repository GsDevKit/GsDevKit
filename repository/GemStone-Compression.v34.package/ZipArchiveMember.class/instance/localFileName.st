accessing
localFileName
	"Answer my fileName in terms of the local directory naming convention"
	"| localName |
	localName := fileName copyReplaceAll: '/' with: FileDirectory slash.
	^(fileName first = $/)
		ifTrue: [ FileDirectory makeAbsolute: localName ]
		ifFalse: [ FileDirectory makeRelative: localName ]"
	^fileName