selecting
ancestorOf: aNode in: aBlock 
	| ancestor |
	ancestor := self ancestorOf: aNode from: children in: aBlock.
	ancestor ifNil: 
		[ancestor := parent childAncestorOf: aNode indexIn: aBlock.
		self refresh].
	ancestor ifNil: [aBlock value: 0].
	^ancestor