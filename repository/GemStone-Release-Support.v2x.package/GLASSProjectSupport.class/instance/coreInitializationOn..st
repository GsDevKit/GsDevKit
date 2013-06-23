bootstrap fileout
coreInitializationOn: stream

	stream nextPutAll:
'
run
ZipFileConstants initialize.
ZipConstants initialize.
ZipWriteStream initialize.
ZipReadStream initialize.
DataStream initialize.
Scanner initialize.
(System myUserProfile symbolList objectNamed: BootstrapSymbolDictionaryName)
    at: #SystemOrganization put: SystemOrganizer new.
MczInstaller initialize.
true
%
'