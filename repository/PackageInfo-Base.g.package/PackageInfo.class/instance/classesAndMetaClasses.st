listing
classesAndMetaClasses
	| baseClasses |
	baseClasses := self classes.
	^baseClasses , (baseClasses collect: [:c | c class])