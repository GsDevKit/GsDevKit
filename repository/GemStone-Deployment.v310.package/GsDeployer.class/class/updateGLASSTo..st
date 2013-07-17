upgrading
updateGLASSTo: versionString
	"load latest ConfigurationOfGLASS package and then update to version <versionString>"
	| glassConfig |

	self deploy: [
		Gofer new
			gemsource: 'MetacelloRepository';
			package: 'ConfigurationOfGlass';
			load.
		((Smalltalk at: #ConfigurationOfGLASS) project version: versionString) load ]
	