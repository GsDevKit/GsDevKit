fileIn/Out
scanFrom: aStream
| doc txt |
doc := GsClassDocumentation newForClass: class.
txt := (GsDocText new) details: aStream nextChunk.
doc documentClassWith: txt.
class _description: doc.
changeStamp ~~ nil ifTrue: [ class commentStamp: changeStamp ]