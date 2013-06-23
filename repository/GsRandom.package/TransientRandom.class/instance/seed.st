Accessing
seed

   "Return the value of the instance variable 'seed'."
   seed == nil 
		ifTrue: [ 
			self initialize.
			self register.
		 ].
   ^seed