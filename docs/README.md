To install GemStone, you should follow the instructions in the install guide (http://seaside.gemstone.com/docs/GS64-InstallGuide-Linux-2.2.pdf). Pay special attention to the information on page 1-3 about setting up shared memory and shared semaphores. It is fine to follow the recommended 75% rule of thumb as the shared memory is only allocated when you actually start a stone.

In order to run seaside you will need to define some environment 
variables. The $GEMSTONE environment variable is expected to refer 
to the base directory of your GemStone installation.

In the following instructions swordfish is used as the password, 
because swordfish is the default password for DataCurator. If you 
change the password for DataCurator, then you would substitute the 
new password for swordfish.


1. The path to this file in a standard installation would be 
   $GEMSTONE/seaside/readme.txt. The rest of this script assumes
   you have cd'ed to $GEMSTONE/seaside.

2. In $GEMSTONE/seaside/etc/gemstone.conf, you will find a number of
   environment variable exports. The other scripts in the bin directory
   depend upon these environment variables being defined. You will
   need to edit this file and define the GEMSTONE variable to point
   to the base directory of you GemStone installation, but first:

     chmod +w gemstone.conf 

   While you are editing gemstone.conf verify that the keyfile (which you
   received from GemStone is located in $GEMSTONE/seaside/etc/gemstone.key. If it is
   not, either move the file, or change the GEMSTONE_KEYFILE definition.

3. In a bash shell, source the defSeaside:

     . defSeaside

   so that the environment variables are defined within your shell.

   defSeaside adds $GEMSTONE/seaside/bin to your path, however, in the 
   following examples I will use a fully qualified pathname for each script.

4. Decide where you would like the Gemstone repository to reside. This 
   is where the extent files, tranlog files, and log files will be
   written. $GEMSTONE/seaside/data is a convenient place to start and 
   is the default location for $GEMSTONE_DATADIR 
   (in $GEMSTONE/seaside/etc/gemstone.conf).
   If you decide to locate the repository somewhere else, change the 
   definition of GEMSTONE_DATADIR in $GEMSTONE/seaside/etc/gemstone.conf.

5. Copy the initial extent and system.conf into the GEMSTONE_DATADIR:

     cp $GEMSTONE/bin/extent0.dbf $GEMSTONE_DATADIR
     chmod +w $GEMSTONE_DATADIR/extent0.dbf
     cp $GEMSTONE/seaside/system.conf $GEMSTONE_DATADIR
     chmod +w $GEMSTONE_DATADIR/system.conf

6. You are now ready to start your stone (the script stops any stone
   that may already be running first):

     $GEMSTONE/seaside/bin/startGemstone

   Run the following command to verify that the stone is running:

     gslist -lc

7. To stop a running stone:

     $GEMSTONE/seaside/bin/stopGemstone

8. Once the stone is running, you need to load the Seaside code into 
   the repository. 

     $GEMSTONE/seaside/bin/loadSeaside

  The errorCount at the end of the run should be 0.

9. At this point you would need to load your own application into the
   repository. The sushi store example is preloaded into the repository
   and the file $GEMSTONE/seaside/bin/initSeaside should be modified
   based on the needs of you application. The file 
   $GEMSTONE/seaside/bin/initSeaside should be run whenever you wish to
   completely initialize/reset your Seaside installation. 

     $GEMSTONE/seaside/bin/initSeaside

   The errorCount at the end of the run should be 0.

10. Now you can start the Seaside server.
   

    If you want to run using Hyper (no Apache required). You will hit port
    50081 to access the http server in this example:

      $GEMSTONE/seaside/bin/startSeaside_Hyper 50081

    The seaside apps can be reached with the following URL:

      http://<hostname>:50081/seaside/examples/counter

    Where <hostname> is the name of your host as returned by `hostname`.

    If you want to run using FastCGI (Apache or equivalent required). You
    will have to configure Apache to forward requests to port 50080 (in 
    this example):

      $GEMSTONE/seaside/bin/startSeaside_FastCGI 50080

    In both cases the script will not return, so you might want to run them
    using screen (see http://www.rackaid.com/resources/tips/linux-screen.cfm).
    
    During development it is convenient to run topaz in the foreground so that 
    the debugger can be used.

11. Once you've set up apache or lighttpd you will want to start running multiple
    vms. For that you can use the runSeasideGems.sh script as follows:

      $GEMSTONE/seaside/bin/runSeasideGems restart

    You may use start, but restart is the most convenient since it cleans up 
    extraneous pid files. To shut down the gems use:

      $GEMSTONE/seaside/bin/runSeasideGems stop

   By default the script launches three FastCGI gems listening on ports 9001-9003.
   To change the number of gems or the ports that they listen on, simply edit the 
   file and modify the following line:

      GEMS="9001 9002 9003"


    
