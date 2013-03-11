#!/bin/bash -x
#
#  GLASS test driver for running GLASS builds on travisCI ... for now the code
#    is hosted elsewhere, but ultimately the packages will be managed here.
#
#  This boyo generates an old-style configuration load script similar to those 
#    used by GemTools and builderCI in before_gemstone.sh, but we're testing
#    a barebones load and test so no need for Metacello Scripting API, to be
#    pre-installed.
#
#      -verbose flag causes unconditional transcript display
#
# Copyright (c) 2013 VMware, Inc. All Rights Reserved <dhenrich@vmware.com>.
#

OUTPUT_PATH="${PROJECT_HOME}/tests/glass.st"

cat - >> $OUTPUT_PATH << EOF
Transcript cr; show: 'travis--->${OUTPUT_PATH}'.
ConfigurationOfGLASS project updateProject.
[
"load core packages, including Metacello Scripting API"
(ConfigurationOfGLASS project version: '${VERSION}') load: #( ${INITIAL_LOADS} ).
"Now load full GLASS configuration using Metacello Scripting API"
Metacello image
	configuration: 'GLASS';
	version: '${VERSION}';
	load ]
    on: Warning, MetacelloSkipDirtyPackageLoad
    do: [:ex |
	(ex isKindOf: MetacelloSkipDirtyPackageLoad)
	    ifTrue: [
                "load over dirty packages"
                ex resume: false ]
	    ifFalse: [ 
                Transcript cr; show: 'Warning: ', ex description.
                ex resume: true ]].
EOF

cat $OUTPUT_PATH

# Inline $BUILDER_CI_HOME/testTravisCI.sh so I can include -G option, etc.
./build.sh -i $ST -G -f "$PROJECT_HOME/tests/before_gemstone.st" -f "$PROJECT_HOME/tests/glass.st" -m -f "$PROJECT_HOME/tests/runGlassTests.st" -o travisCI
if [[ $? != 0 ]] ; then
  echo "ERROR: $(basename $0)"
  cd "${BUILD_PATH}/travisCI/"
  $BUILDER_CI_HOME/buildImageErrorCheck.sh # dump Transcript on error and exit
  if [[ $? != 0 ]] ; then exit 1; fi
  $BUILDER_CI_HOME/dumpTranscript.sh
  exit 1
fi
cd "${BUILD_PATH}/travisCI/"
$BUILDER_CI_HOME/buildImageErrorCheck.sh # dump Transcript on error and exit
if [[ $? != 0 ]] ; then exit 1; fi
$BUILDER_CI_HOME/buildTravisStatusCheck.sh "$@" # dump Transcript on failed tests and exit
if [[ $? != 0 ]] ; then exit 1; fi

