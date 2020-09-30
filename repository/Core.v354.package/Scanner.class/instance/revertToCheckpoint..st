expression types
revertToCheckpoint: checkpoint
	"Revert to the state when checkpoint was made."

	| myCopy |
	myCopy _ checkpoint first.
	1 to: self class instSize do:
		[:i | self instVarAt: i put: (myCopy instVarAt: i)].
	source _ checkpoint second.
	currentComment _ checkpoint third