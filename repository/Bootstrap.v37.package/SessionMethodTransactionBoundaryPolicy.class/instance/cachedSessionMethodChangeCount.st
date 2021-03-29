private
cachedSessionMethodChangeCount

	^self cacheDictionary at: self sessionMethodChangeCountCacheName otherwise: -1