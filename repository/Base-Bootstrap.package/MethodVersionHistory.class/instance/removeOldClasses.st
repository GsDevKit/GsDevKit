actions
removeOldClasses
	self queryOldClasses do: [:cl | dictionary removeKey: cl]