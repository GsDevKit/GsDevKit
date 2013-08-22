private
rawClasses
	^ (self classNames select: [:ea | (environment at: ea ifAbsent: []) ~~ nil ]) collect: [:cl | environment at: cl]