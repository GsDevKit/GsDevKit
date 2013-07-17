running
runCase

	MCPlatformSupport suspendAutoCommitAndMigrationDuring: [
		SystemChangeNotifier uniqueInstance doSilently: [ 
			super runCase ]].