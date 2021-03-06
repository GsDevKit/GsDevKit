private
promptForUserNamePassword
  | pass |
  pass := ''.
  [ pass isEmpty ]
    whileTrue: [ 
      | answer |
      answer := (GsTextInteraction
        prompt: 'User name for ' , location
        template: self user) signal.
      answer isEmpty
        ifTrue: [ ^ password ]
        ifFalse: [ self user: answer ].
      pass := (GsTextInteraction
        requestPassword: 'Password for ' , self user , ' at ' , location) signal ].
  ^ pass