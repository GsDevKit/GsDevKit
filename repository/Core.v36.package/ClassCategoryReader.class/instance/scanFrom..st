fileIn/Out
scanFrom: aStream 
	"File in methods from the stream, aStream."
	| methodText |
	[methodText := aStream nextChunk.
	 methodText size > 0]
		whileTrue:
		[ 
        class compileMethod: methodText withGemstoneLineEndings category: category.
		changeStamp ~~ nil 
          ifTrue: [class setStamp: changeStamp forMethod: (Behavior parseSelector: methodText for: class)]
		]