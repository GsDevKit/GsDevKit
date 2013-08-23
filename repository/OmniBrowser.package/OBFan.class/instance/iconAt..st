accessing
iconAt: index
	^ (self children at: index ifAbsent: [^nil]) icon