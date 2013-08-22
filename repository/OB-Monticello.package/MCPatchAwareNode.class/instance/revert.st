as yet unclassified
revert
	| loader |
	loader _ MCPackageLoader new.
	self operation applyInverseTo: loader.
	loader loadWithName: 'Installed Patches'