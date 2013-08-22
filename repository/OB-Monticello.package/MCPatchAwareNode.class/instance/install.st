as yet unclassified
install
	| loader |
	loader _ MCPackageLoader new.
	self operation applyTo: loader.
	loader loadWithName: 'Installed Patches'