private
compressDataTo: aStream
    "Copy my deflated data to the given stream."
    | startPos endPos compressedBytes unCompressedBytes |

    startPos := aStream position.

    unCompressedBytes := (self readRawChunk: uncompressedSize) asByteArray.
    compressedBytes :=  unCompressedBytes _compress.
    compressedBytes == nil 
        ifTrue: [ 
            "needed to undo the previous readRawChunk:"
            self rewindData.
            ^self stCompressDataTo: aStream ].
    aStream nextPutAll: compressedBytes asString.
    endPos := aStream position.
    compressedSize := endPos - startPos.
    crc32 := unCompressedBytes _computeCRC32: 0.