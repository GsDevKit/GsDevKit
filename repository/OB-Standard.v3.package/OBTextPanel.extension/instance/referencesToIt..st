*ob-standard-cmds
referencesToIt: aClassName 
	| class | 
	class := self environment at: aClassName ifAbsent: [^false].
	class isBehavior ifFalse: [^false].
	OBReferencesBrowser browseRoot: (OBClassNode on: class).
	^true