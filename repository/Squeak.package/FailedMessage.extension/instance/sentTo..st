*squeak
sentTo: receiver
	"answer the result of sending this message to receiver"

	^ receiver perform: self selector withArguments: self arguments