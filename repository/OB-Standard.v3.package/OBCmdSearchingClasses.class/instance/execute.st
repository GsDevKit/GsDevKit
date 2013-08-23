execution
execute
	| class |
	class := self requestClassToUser.
	class ifNotNil: [self select: class asNode with: requestor announcer].