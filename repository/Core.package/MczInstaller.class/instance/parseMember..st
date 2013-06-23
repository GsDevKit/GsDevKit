as yet unclassified
parseMember: fileName
	| tokens |
	tokens _ (self scanner scanTokens: (zip contentsOf: fileName)) first.
	^ self associate: tokens