testing
canInform

	^(self clientForwarderMap at:#inform ifAbsent: [ nil ]) ~~ nil