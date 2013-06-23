private
sendMailFrom

	^self sendCrlfString: ('MAIL FROM:<', self from add: '>'; yourself ).
