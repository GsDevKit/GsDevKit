*gemstone-compression
install
	| sources |
	zip _ ZipArchive new.
	zip readFrom: stream.
	self checkDependencies ifFalse: [^false].
	"don't recordVersionInfo ... the extractVersionInfo method is recursive ... we don't depend upon the versionInfo recorded by
	 McsInstaller anyway .... once bootstrap with MczInstaller is done, we reload using Monticello"
	"self recordVersionInfo."
	sources _ (zip membersMatching: 'snapshot/*') 
				asSortedCollection: [:a :b | a fileName < b fileName].
	sources do: [:src | self installMember: src].
