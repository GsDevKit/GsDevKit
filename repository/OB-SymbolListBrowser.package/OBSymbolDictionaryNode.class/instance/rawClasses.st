private
rawClasses
	^ self symbolDictionary values select: [:global | global isBehavior ]