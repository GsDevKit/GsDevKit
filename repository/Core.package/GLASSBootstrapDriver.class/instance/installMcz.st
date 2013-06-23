installing
installMcz

	"Boot monticello using mcz loader"
	self 
		mczInstall: self coreFilenames silently: false;
		mczInstall: self monticelloFilenames silently: true.
	self postMczInstallInitialization.
