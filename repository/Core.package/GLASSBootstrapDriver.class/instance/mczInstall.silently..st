mcz installing
mczInstall: filenames silently: silently

	| basePath |
	basePath := self repositoryDirectory.
	silently
		ifTrue: [
			self systemChangeNotifierClass uniqueInstance doSilently: [
				filenames do: [:filename |
					self log: 'MCZ load: ', filename.
					self mczInstallerClass
						installFileNamed: basePath, filename
						in: self bootStrapSymbolDictionary ]]]
		ifFalse: [
			filenames do: [:filename |
				self log: 'MCZ load: ', filename.
				self mczInstallerClass
					installFileNamed: basePath, filename
					in: self bootStrapSymbolDictionary ]].
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].