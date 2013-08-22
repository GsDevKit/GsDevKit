private
checkForNewerVersions
	| newer |
	newer := self workingCopy possiblyNewerVersionsIn: self repository.
	^ newer isEmpty or: [ | list |
		list := String streamContents: [:stream |
			newer do: [:ea | stream nextPutAll: ea, String cr]].
		self confirm: 'CAUTION! These versions in the repository may be newer:', 
			String cr, list,
			'Do you really want to save this version?'].