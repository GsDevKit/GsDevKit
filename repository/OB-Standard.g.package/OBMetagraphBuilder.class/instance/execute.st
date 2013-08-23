execution
execute
	((self class allSelectors select: [:ea | ea beginsWith: 'populate']) asSortedArray)
		do: [:selector | self perform: selector].
	^ self root