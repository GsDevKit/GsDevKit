execution
execute
	| res defectSelectors |
	res := self buildSuite run.
	res hasPassed 
		ifTrue: [self successNotification]
		ifFalse: 
			[defectSelectors := res defects collect: [:ts | ts selector].
			defectSelectors size > 2
				ifTrue: [defectSelectors := (defectSelectors copyFrom: 1 to: 2), {'...'}].
			defectSelectors := defectSelectors inject: ''
						into: [:sum :el | sum , ' ' , el asString].
			self inform: res printString , ' => ' , defectSelectors]