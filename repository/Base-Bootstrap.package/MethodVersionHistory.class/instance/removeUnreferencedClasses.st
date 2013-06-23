actions
removeUnreferencedClasses
	| userProfile |
	userProfile := System myUserProfile.
	dictionary keys do: [:cl | (userProfile objectNamed: cl thisClass name) isNil ifTrue: [dictionary removeKey: cl]]