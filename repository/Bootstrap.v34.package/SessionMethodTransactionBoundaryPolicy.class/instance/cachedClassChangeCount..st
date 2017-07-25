private
cachedClassChangeCount: anInteger

	^self cacheDictionary at: self classChangeCountCacheName put: anInteger