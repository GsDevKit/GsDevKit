converting
asFileDirectory
	"Answer a FileDirectory representing the same directory I represent."
	
	| sd |
	sd := self sourceDirectory.
	^sd class on: (sd fullNameFor: self name)