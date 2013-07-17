migration
classesWithClassHistory
	"GsDeployer new classesWithClassHistory"

	| symbolList classes |
	symbolList := System myUserProfile symbolList.
	classes := IdentitySet new.
	symbolList do: [:symbolDict | 
		(self excluding includes: symbolDict)
			ifFalse: [ 
				symbolDict do: [:eachGlobal | 
					((self excluding includes: eachGlobal) not and: [ eachGlobal isClass ])
						ifTrue: [ | history |
							history := eachGlobal classHistory.
							history last == eachGlobal 
								ifTrue: [ 1 to: history size - 1 do: [:i | classes add: (history at: i) ] ] ] ] ] ].
	^Array withAll: classes "sort in oop order: Bug41254"
