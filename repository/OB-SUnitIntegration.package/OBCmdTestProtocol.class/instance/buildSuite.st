private
buildSuite
	| selectorUnderTheCategory tests |
	selectorUnderTheCategory := target theClass organization listAtCategoryNamed: target category.
	tests := target theClass suite tests select: [:each| selectorUnderTheCategory includes: each selector].
	^ TestSuite new addTests: tests