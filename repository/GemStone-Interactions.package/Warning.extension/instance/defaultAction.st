*gemstone-interactions
defaultAction
  "The user should be notified of the occurrence of an exceptional
	occurrence and given an option of continuing or aborting the
	computation. The description of the occurrence should include
	any text specified as the argument of the #signal: message.

	It is not so clear how this would be done in GemStone."

  (GsConfirmInteraction
    prompt:
      'Warning: ' , self description withoutGemstoneLineEndings
        , ' Press ''Proceed'' to continue.'
    confirm: 'Proceed'
    cancel: 'Debug') signal == true
    ifTrue: [ ^ super defaultAction ].
  self halt: 'Debug Warning: ' , self description