as yet unclassified
conflicts
	^ self merger conflicts collect: [:ea | MCConflictNode new conflict: ea]