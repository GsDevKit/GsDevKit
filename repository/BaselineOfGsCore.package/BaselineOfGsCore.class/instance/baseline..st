baseline
baseline: spec
  <baseline>
  spec
    for: #'gemstone'
    do: [ 
      spec
        package: 'Core';
        package: 'Base-Bootstrap' with: [ spec requires: #('Core') ];
        package: 'Bootstrap' with: [ spec requires: #('Base-Bootstrap') ];
        package: 'GemStone-ANSI-Streams' with: [ spec requires: #('Core') ];
        package: 'GemStone-Indexing-Extensions'
          with: [ spec requires: #('Bootstrap') ];
        package: 'Sport'
          with: [ 
              spec
                requires: #('Bootstrap');
                repository: 'http://seaside.gemstone.com/ss/hyper' ];
        package: 'Squeak' with: [ spec requires: #('Sport') ];
        package: 'Regex-Core' with: [ spec requires: #('Core' 'Squeak') ];
        package: 'Regex-Tests-Core' with: [ spec requires: #('Regex-Core') ];
        yourself ].
  spec
    for: #'gs2.x'
    do: [ 
      spec
        package: 'GsRandom' with: [ spec requires: #('Base-Bootstrap') ];
        package: 'GemStone-Exceptions'
          with: [ spec requires: #('Bootstrap' 'Sport') ];
        package: 'Sport' with: [ spec requires: #('GsRandom') ];
        yourself ].
  spec
    for: #'gs2.3.x'
    do: [ 
      spec
        package: 'Core' with: [ spec preLoadDoIt: #'preLoad02322' ];
        package: 'GemStone-23x-ANSI-Streams'
          with: [ spec requires: #('GemStone-ANSI-Streams') ];
        package: 'GemStone-23x-GreaseCompat' with: [ spec requires: #('Core') ];
        package: 'GemStone-23x-Exceptions'
          with: [ spec requires: #('GemStone-Exceptions') ];
        package: 'Squeak' with: [ spec postLoadDoIt: #'postLoad233' ];
        yourself ].
  spec
    for: #'Issue29FixANeeded'
    do: [ 
      spec
        package: 'GemStone-244-Exceptions'
        with: [ spec requires: #('GemStone-Exceptions') ] ].
  spec
    for: #'Issue29FixBNeeded'
    do: [ 
      spec
        package: 'GemStone-245-Exceptions'
        with: [ spec requires: #('GemStone-Exceptions') ] ].
  spec for: #'gs3.x' do: [  ]