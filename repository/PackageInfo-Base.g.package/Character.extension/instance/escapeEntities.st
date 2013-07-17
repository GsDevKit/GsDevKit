*packageinfo-base
escapeEntities
	#($< '&lt;' $> '&gt;' $& '&amp;') pairsDo:
		[:k :v |
		self = k ifTrue: [^ v]].
	^ String with: self