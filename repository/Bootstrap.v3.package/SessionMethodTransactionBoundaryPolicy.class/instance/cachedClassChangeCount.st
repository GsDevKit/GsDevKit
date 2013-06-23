private
cachedClassChangeCount

	^self cacheDictionary at: self classChangeCountCacheName otherwise: -1
