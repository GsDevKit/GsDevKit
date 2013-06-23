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
                file: 'Sport3.010-dkh.28';
                requires: #('Bootstrap');
                repository: 'http://seaside.gemstone.com/ss/hyper' ];
        package: 'Squeak' with: [ spec requires: #('Sport') ];
        package: 'Regex-Core' with: [ spec requires: #('Core' 'Squeak') ];
        package: 'Regex-Tests-Core' with: [ spec requires: #('Regex-Core') ];
        yourself.
      spec
        package: 'SMTPMail' with: [ spec requires: #('default') ];
        package: 'GemStone-Release-Support' with: [ spec requires: #('Core') ];
        yourself.
      spec
        group: 'default'
          with:
            #('Core' 'Base-Bootstrap' 'Bootstrap' 'GemStone-ANSI-Streams' 'GemStone-Indexing-Extensions' 'Sport' 'Regex-Core' 'Regex-Tests-Core' 'GemStone-Release-Support');
        yourself ].
  spec
    for: #'gs2.x'
    do: [ 
      spec
        package: 'GsRandom' with: [ spec requires: #('Base-Bootstrap') ];
        package: 'GemStone-Exceptions'
          with: [ spec requires: #('Bootstrap' 'Sport') ];
        package: 'GemStone-Release-Support'
          with: [ spec file: 'GemStone-Release-Support.v2x' ];
        package: 'Sport' with: [ spec requires: #('GsRandom') ];
        yourself.
      spec
        group: 'default' with: #('GsRandom' 'GemStone-Exceptions');
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
        yourself.
      spec
        group: 'default'
          with:
            #('GemStone-23x-ANSI-Streams' 'GemStone-23x-GreaseCompat' 'GemStone-23x-Exceptions');
        yourself ].
  spec
    for: #'Issue29FixANeeded'
    do: [ 
      spec
        package: 'GemStone-244-Exceptions'
        with: [ spec requires: #('GemStone-Exceptions') ].
      spec
        group: 'default' with: #('GemStone-244-Exceptions');
        yourself ].
  spec
    for: #'Issue29FixBNeeded'
    do: [ 
      spec
        package: 'GemStone-245-Exceptions'
        with: [ spec requires: #('GemStone-Exceptions') ].
      spec
        group: 'default' with: #('GemStone-245-Exceptions');
        yourself ].
  spec
    for: #'gs3.x'
    do: [ 
      spec
        package: 'Core' with: [ spec file: 'Core.v3' ];
        package: 'Base-Bootstrap' with: [ spec file: 'Base-Bootstrap.v3' ];
        package: 'Bootstrap' with: [ spec file: 'Bootstrap.v3' ];
        package: 'Sport' with: [ spec file: 'Sport3.010.v3-dkh.29' ];
        package: 'Squeak' with: [ spec file: 'Squeak.v3' ];
        yourself ].
  spec
    for: #'maglev'
    do: [ spec package: 'Bootstrap' with: [ spec file: 'Bootstrap.maglev' ] ]