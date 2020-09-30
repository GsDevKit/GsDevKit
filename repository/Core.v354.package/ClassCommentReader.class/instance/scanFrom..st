fileIn/Out
scanFrom: aStream
  class comment: aStream nextChunk.
  changeStamp ~~ nil
    ifTrue: [ class commentStamp: changeStamp ]