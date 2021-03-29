*bootstrap
createPragmaFrom: aPragmaArray for: aGsMethod
    
	| pragma args pragmas keyword |
	aPragmaArray isEmpty ifTrue: [ ^nil ].
	pragmas := Array new.
	1 to: aPragmaArray size by: 2 do: [:i |
		keyword := aPragmaArray at: i.
		args := aPragmaArray at: i + 1.
		args == nil ifTrue: [ args := #() ].
		pragma := Pragma keyword: keyword arguments: args.
		pragma setMethod: aGsMethod.
		pragmas add: pragma].
	^pragmas