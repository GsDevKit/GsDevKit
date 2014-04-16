private
sentButNotImplemented
  | sent implemented |
  sent := IdentitySet new.
  implemented := IdentitySet new.
  ClassOrganizer cachedOrganizer classes do: [:class | | metaClass |
    class selectors do: [:sel | | meth |
      implemented add: sel.
      meth := class compiledMethodAt: sel.
      sent addAll: meth _selectorPool ].
    metaClass := class class.
    metaClass selectors do: [:sel | | meth |
      implemented add: sel.
      meth := metaClass compiledMethodAt: sel.
      sent addAll: meth _selectorPool ]].
  ^sent - implemented