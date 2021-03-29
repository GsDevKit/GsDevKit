private
cachedSessionMethodChangeCount: anInteger

	^self cacheDictionary at: self sessionMethodChangeCountCacheName put: anInteger