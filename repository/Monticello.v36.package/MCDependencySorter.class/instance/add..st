building
add: anItem
	| requirements |
	requirements _ self unresolvedRequirementsFor: anItem.
	requirements remove: 'nil' ifAbsent: [  ].	"nil superclass is satisfied"
	requirements isEmpty
		ifTrue: [ self addToOrder: anItem ]
		ifFalse: [ self addRequirements: requirements for: anItem ]