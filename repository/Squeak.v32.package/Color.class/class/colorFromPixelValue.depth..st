instance creation
colorFromPixelValue: p depth: d
	"Convert a pixel value for the given display depth into a color."
	"Details: For depths of 8 or less, the pixel value is simply looked up in a table. For greater depths, the color components are extracted and converted into a color."

	| r g b alpha |
	d = 8 ifTrue: [^ IndexedColors at: (p bitAnd: 16rFF) + 1].
	d = 4 ifTrue: [^ IndexedColors at: (p bitAnd: 16r0F) + 1].
	d = 2 ifTrue: [^ IndexedColors at: (p bitAnd: 16r03) + 1].
	d = 1 ifTrue: [^ IndexedColors at: (p bitAnd: 16r01) + 1].

	(d = 16) | (d = 15) ifTrue: [
		"five bits per component"
		r _ (p bitShift: -10) bitAnd: 16r1F.
		g _ (p bitShift: -5) bitAnd: 16r1F.
		b _ p bitAnd: 16r1F.
		(r = 0 and: [g = 0]) ifTrue: [
			b = 0 ifTrue: [^Color transparent].
			b = 1 ifTrue: [^Color black]].
		^ Color r: r g: g b: b range: 31].

	d = 32 ifTrue: [
		"eight bits per component; 8 bits of alpha"
		r _ (p bitShift: -16) bitAnd: 16rFF.
		g _ (p bitShift: -8) bitAnd: 16rFF.
		b _ p bitAnd: 16rFF.
		alpha _ p bitShift: -24.
		alpha = 0 ifTrue: [^Color transparent].
		(r = 0 and: [g = 0 and: [b = 0]])  ifTrue: [^Color transparent].
		alpha < 255
			ifTrue: [^ (Color r: r g: g b: b range: 255) alpha: (alpha asFloat / 255.0)]
			ifFalse: [^ (Color r: r g: g b: b range: 255)]].

	d = 12 ifTrue: [
		"four bits per component"
		r _ (p bitShift: -8) bitAnd: 16rF.
		g _ (p bitShift: -4) bitAnd: 16rF.
		b _ p bitAnd: 16rF.
		^ Color r: r g: g b: b range: 15].

	d = 9 ifTrue: [
		"three bits per component"
		r _ (p bitShift: -6) bitAnd: 16r7.
		g _ (p bitShift: -3) bitAnd: 16r7.
		b _ p bitAnd: 16r7.
		^ Color r: r g: g b: b range: 7].

	self error: 'unknown pixel depth: ', d printString