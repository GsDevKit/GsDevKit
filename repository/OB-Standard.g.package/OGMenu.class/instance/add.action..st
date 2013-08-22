accessing
add: label action: aSelector

	"nil target signals to Squeak that #add:action: is to be used when reconstructing the menu"
	self items add: (OGMenuItem label: label target: nil selector: aSelector)