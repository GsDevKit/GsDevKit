*ob-monticello
versionNodes
	^ self dictionary keys collect: [:versionInfo | MCDictionaryBasedVersionNode versionName: versionInfo name repository: self]	