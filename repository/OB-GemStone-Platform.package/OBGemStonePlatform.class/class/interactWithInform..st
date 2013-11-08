interaction
interactWithInform: aGsInformInteraction
  | obRequest |
  obRequest := OBInformRequest new
    message: aGsInformInteraction message;
    yourself.
  ^ self handleInformRequest: obRequest