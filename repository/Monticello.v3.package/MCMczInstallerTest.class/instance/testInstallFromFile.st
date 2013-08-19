as yet unclassified
testInstallFromFile
	| name |
	name := FileDirectory default fullNameFor: self fileName asString.
	MCPlatformSupport writeStreamForFileNamed: name replace: true do: [:stream |
		MCMczWriter fileOut: expected on: stream].
	MczInstaller installFileNamed: name.
	self assertNoChange.