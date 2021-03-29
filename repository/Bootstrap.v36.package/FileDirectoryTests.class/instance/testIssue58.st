tests
testIssue58
  "https://github.com/GsDevKit/GsDevKit/issues/58"

  | main |
  (main := ServerFileDirectory default directoryNamed: self mainSubdirectoryName)
    exists
    ifTrue: [ main recursiveDelete ].
  ServerFileDirectory default createDirectory: self mainSubdirectoryName.
  main := ServerFileDirectory default directoryNamed: self mainSubdirectoryName.
  main newFileNamed: 'newFile' do: [ :file | file nextPutAll: 'Hello World' ].
  main
    readOnlyFileNamed: 'newFile'
    do: [ :file | self assert: file contents = 'Hello World' ].
  main
    readOnlyFileNamed: 'newFile'
    do: [ :file | 
      self deny: file atEnd.
      file next: 11.
      self assert: file atEnd ]