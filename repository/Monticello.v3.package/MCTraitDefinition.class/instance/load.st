installing
load
	^ self.
	"unsupported on gemstone
	
	 self createClass ifNotNil: [:trait |
		self hasComment ifTrue: [trait classComment: comment stamp: commentStamp]]"