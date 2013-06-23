*core-squeak
methodsDo: aBlock
	"evaluates aBlock for each method in receiver's method dictionaries. 
      Returns the receiver.
   
      aBlock should be a two argument block
      expecting args  selectorSymbol ,   aGsNMethod ."

	self _methodDict keysAndValuesDo: aBlock