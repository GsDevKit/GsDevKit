testing
methodWithBangs
	^ '
	^ ReadStream on: 
''MCRevisionInfo packageName: ''MonticelloCompatibilityTest''!!
MCOrganizationDeclaration categories: 
  #(
  ''Monticello-Tests-Mocks'')!!

MCClassDeclaration
  name: #MCMockClassD
  superclassName: #Object
  category: #''Monticello-Tests-Mocks''
  instVarNames: #()
  comment: ''''!!

MCMethodDeclaration className: #MCMockClassD selector: #one category: #''as yet unclassified'' timeStamp: ''cwp 7/8/2003 21:21'' source: 
''one
	^ 1''!!
''
'
