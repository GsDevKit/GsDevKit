as yet unclassified
dateAndTimes	| dateAndTimes |	dateAndTimes := OrderedCollection new.	self scheduleDo: [ :e | dateAndTimes add: e ].	^ dateAndTimes asArray.