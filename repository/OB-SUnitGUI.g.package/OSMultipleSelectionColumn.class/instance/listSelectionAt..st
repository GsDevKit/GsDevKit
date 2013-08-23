callbacks
listSelectionAt: anIndex

	^self multipleSelection at: anIndex ifAbsent: [false]