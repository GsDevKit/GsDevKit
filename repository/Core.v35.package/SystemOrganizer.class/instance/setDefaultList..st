private
setDefaultList: aSortedCollection

	categoryDict _ Dictionary new.
	aSortedCollection do: [:each | self addCategory: each ]