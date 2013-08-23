requests
messageCategory
	class == nil ifTrue: [ ^'' ].
	^ class categoryOfSelector: selector