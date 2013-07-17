listing
overrideMethods
	^ self extensionMethods select: [:ea | self isOverrideMethod: ea]