*bootstrap
should: aBlock raise: anExceptionalEvent withExceptionDo: anotherBlock

	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withExceptionDo: anotherBlock)