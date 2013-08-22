*ob-monticello
versionNodes
	^ self readableFileNames collect: [:ea | MCFileBasedVersionNode fileName: ea repository: self]		