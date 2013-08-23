accessing
versionNumber
	^ [(self versionName copyAfterLast: $.) asNumber] ifError: [0]