http
readStreamForUrl: url user: user passwd: password do: aBlock
  | contents retries |
  retries := 0.
  [ retries <= 3 ]
    whileTrue: [ 
      [ 
      retries := retries + 1.
      contents := HTTPSocket
        httpGet: url
        args: nil
        user: user
        passwd: password.
      retries := 4 ]
        on: Error
        do: [ :ex | 
          retries > 3
            ifTrue: [ ex pass ].
          Transcript
            cr;
            show:
                'Retrying ' , url printString , ' (' , retries printString , ') '
                    , ex description printString ] ].
  ^ contents isString
    ifTrue: [ nil ]
    ifFalse: [ aBlock value: contents ]