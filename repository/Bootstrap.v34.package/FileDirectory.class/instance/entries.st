operations
entries
  | ar contents |
  ar := Array new.
  contents := GsFile
    contentsAndTypesOfDirectory: self pathString
    onClient: self onClient.
  1 to: contents size by: 2 do: [ :i | 
    | path dirFlag modTime sz nm entry |
    path := (contents at: i) asUnicodeString.
    nm := (path findTokens: '/\') last.
    (nm ~= '.' _and: [ nm ~= '..' ])
      ifTrue: [ 
        dirFlag := (contents at: i + 1) not.
        modTime := GsFile _fileModTime: path onClient: self onClient.
        sz := dirFlag
          ifTrue: [ 0 ]
          ifFalse: [ GsFile _sizeOf: path onClient: self onClient ].
        entry := (DirectoryEntry
          name: nm
          creationTime: modTime
          modificationTime: modTime
          dirFlag: dirFlag
          fileSize: sz)
          sourceDirectory: self;
          yourself.
        ar add: entry ] ].
  ^ ar