testing
isActive

	^(requestor isSelected: target) and: [ target notNil and: [ target isTestNode ] ]