accessing
renameCategory: old toBe: new
	"Rename a category. No action if new name already exists, or if old name does not exist."
 
	| oldCat newCat ar |
	oldCat := old asString.
	newCat := old asString.
	(self categoryDict at: newCat ifAbsent: []) ~~ nil ifTrue: [ ^self ].
	(ar := self categoryDict at: oldCat ifAbsent: []) == nil ifTrue: [ ^self ].
	self categoryDict removeKey: oldCat.
	self categoryDict at: newCat put: ar