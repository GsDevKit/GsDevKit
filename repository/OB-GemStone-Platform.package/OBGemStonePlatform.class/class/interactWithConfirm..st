interaction
interactWithConfirm: aGsConfirmInteraction
  | obRequest |
  obRequest := OBConfirmationRequest
    newPrompt: aGsConfirmInteraction prompt
    confirm: aGsConfirmInteraction confirm
    cancel: aGsConfirmInteraction cancel.
  ^ self handleConfirmationRequest: obRequest