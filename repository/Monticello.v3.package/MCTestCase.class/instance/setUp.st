running
setUp

	saveAutoMigrate := MCPlatformSupport autoMigrate ifNil: [false].
	monticelloPackageModified := self monticelloPackage modified.
