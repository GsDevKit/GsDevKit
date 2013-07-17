preamble/postscript
preamble

	^ preamble ifNil: [preamble _ StringHolder new contents: '"below, add code to be run before the loading of this package"'].