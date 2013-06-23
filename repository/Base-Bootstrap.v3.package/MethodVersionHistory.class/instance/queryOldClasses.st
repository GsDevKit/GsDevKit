private
queryOldClasses
	"return all keys which are obsolete (e.g. old classes from the class history)"
	| userProfile |
	userProfile := System myUserProfile.
	^dictionary keys select: [:cl | 
		cl isMeta
			ifTrue: [(userProfile objectNamed: cl thisClass name) notNil and: [cl ~~ (userProfile objectNamed: cl thisClass name) class ]]
			ifFalse: [(userProfile objectNamed: cl name) notNil and: [cl ~~ (userProfile objectNamed: cl name) ]]]