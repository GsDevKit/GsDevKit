tokenizing dtd
nextExternalId
	| extDefType systemId dir |
	extDefType _ self nextLiteral.
	extDefType = 'PUBLIC'
		ifTrue: [
			self skipSeparators.
			self nextPubidLiteral.
			self skipSeparators.
			self peek == $>
				ifFalse: [
					systemId _ self nextSystemLiteral]].

	extDefType = 'SYSTEM'
		ifTrue: [
			self skipSeparators.
			systemId _ self nextSystemLiteral].

	systemId
		ifNil: [^nil].

	"The rest of this method only applies if we're reading aFileStream"
	(self topStream isKindOf: GsFile)
		ifFalse: [^''].
	dir _ self topStream directory.
	^(dir fileExists: systemId)
		ifTrue: [(dir readOnlyFileNamed: systemId) contentsOfEntireFile]
		ifFalse: ['']