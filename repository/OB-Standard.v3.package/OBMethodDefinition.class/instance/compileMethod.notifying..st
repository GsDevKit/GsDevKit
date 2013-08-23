compiling
compileMethod: aText notifying: aController
	self compileClass: self theClass.
	self compileCategory: self category.
	self compileText: aText.
	^ (self confirmMethod: aText)
		ifTrue: [| result |
			[result := self theClass 
					compileMethod: aText 
					category: self category
					using: GsSession currentSession symbolList ]
				on: UndefinedSymbolNotification
				do: [:ex | 
					"treat undefined symbols as compile errors"
					ex resume: false].
			(self getSelector: aText) asSymbol]
		ifFalse: [nil]