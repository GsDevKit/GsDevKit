private
withPragmasIn: aClass do: aBlock
	(aClass methodDictForEnv: 0) keysAndValuesDo: [ :selector :method | method pragmas do: aBlock ].
