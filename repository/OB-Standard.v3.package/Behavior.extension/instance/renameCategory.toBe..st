*ob-standard-accessing
renameCategory: oldCatString toBe: newCatString
  "Rename a category. No action if new name already exists, or if old name does not exist."

  (self includesCategory: oldCatString) ifFalse: [ ^self ].
  (self includesCategory: newCatString) ifTrue: [ ^self ].
  ^self renameCategory: oldCatString to: newCatString