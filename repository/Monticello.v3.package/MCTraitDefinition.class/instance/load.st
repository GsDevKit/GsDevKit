installing
load
	^ self error: 'unsupported on GemStone'
	"
	 self createClass ifNotNil: [:trait |
		self hasComment ifTrue: [trait classComment: comment stamp: commentStamp]]"