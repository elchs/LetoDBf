

                              __         __        ____  ____  __
                             / /   ___  / /_____  / __ \/ __ )/ _|
                            / /   / _ \/ __/ __ \/ / / / __  | |_
                           / /___/  __/ /_/ /_/ / /_/ / /_/ /|  _|
                          /_____/\___/\__/\____/_____/_____/ |_| ork


 # Welcome to LetoDBf
 please note the trailing *f*, this is the *elch fork* of the famous LetoDB Database Server,
 for the origin see: https://sourceforge.net/p/letodb/code/ci/master/tree/
 or visit the original inventor: http://kresin.ru/en/letodb.html

 LetoDBf is like the origin a multiplatform, high performance database server with data stored in DBF
 tables. It is programmed mostly in pure 'Ansi-C' by extensively using the underlaying Harbour database
 engine ( http://harbour-project.org )

 The cause of this fork was to get the freedom of removing over many years accumulated legacy technics,
 and to continue, to improve and to enhance based on latest development status, without the strain to
 keep internal backward compatibility to older versions.
 Main goal of this fork is to use LetoDBf with newest Harbour and to max out its possibilities.

 Aside a sligthly, but overall rework of the internal communication between Client and Server,
 a bunch of new features and capabilties is added, to let Harbour DBF engine show its' muscles.
 Expect extendend locking schemes, share-and-lockable DBF tables in RAM ( HbMemIO ), all DBF on demand
 combine-able with 3 memofield types, new extended field attributes like autoincrement, etc ...

 The LetoDBf server file open mode: No_Save_WA is fully reworked: now the workareas are opened in exact
 same workarea-ID and ALIAS as at client side, plus client relations are active at server:
 this will e.g. allow indexing keys/ filtering rules on relationed fields of other workareas in this mode.
 Improved is the use of the server variables system, it allows really tricky filter conditions.
 This mode shell be the mode, if you need to execute server side UDF functions, where you now even can
 start an UDF in its own thread, working from then on independent from your connection as a new
 'headless connection'

 'Under the hood' works now a *TWO-socket* TCP connection ( if client application is MultiThread compiled ),
 enabling e,g, much faster data writes to the server.
 Available is on demand an extreme fast LZ4 compress- and Blowfish-encrypt-able network traffic.
 LetoDBf client lib ( used by your application ) became fully MultiThread save, each thread opens its own
 connection to the server. Threads, threads threads .., wherever you look.

 You should be able to use this fork likely you already used LetoDB --  BUT YOU CAN NOT MIX THEM.
 As the internal communication had changed, this LetoDBf server will only 'understand' client
 applications linked with this LetoDBf client library.

 ! Most important: NO WARRANTY from me on nothing -- decide yourself if LetoDBf fulfills your needs !
 For the case you wish ( reliable quick ) personal! support from me, we should talk about 'donation' :-)
 So far YOU are the only responsible one for all what happens with and around LetoDBf at your places.


Contents
--------

1. Directory structure
2. Building binaries
   2.1 the recommended way ( via hbmk2 )
   2.2 Borland Win32 C compiler
   2.3 MS Visual C compiler
   2.4 Mingw C compiler
   2.5 Addition functions support
   2.6 Codepage support
3. Running and stopping server
4. Server configuration
   4.1 letodb.ini
   4.2 Authentication
5. Features of work with the letodb server
   5.1 Connecting to the server from client programs
   5.2 Filters
   5.3 Database driver
   5.4 Special Data Files in RAM
6. Variables management
7. Functions list
   7.1 Connection management functions
   7.2 Transaction functions
   7.3 Additional functions for current workarea
   7.4 Additional rdd functions
   7.5 Setting client paramenter
   7.6 File functions
   7.7 Management functions
   7.8 User account management functions
   7.9 Server variable management functions
   7.10 Calling udf-functions on the server
   7.11 Functions for bitmap filters
8. Management utility
9. Server-side functions
A. Internals


      1. Directory structure

      bin/          -    server executable file
      doc/          -    documentation
      include/      -    source header files
      lib/          -    rdd library
      source/
          client/   -    client RDD lib sources
          common/   -    some common source files to server and client
          client/   -    server sources
          3rd/      -    third party source
          3rd/lz4   -    LZ4 compression library
      tests/        -    test programs, samples
      utils/
          manager/  -    server management utilities
          backup/   -    demo of backup-ing a running server


      2. Building binaries

 The letodb server and client library can be compiled only by the Harbour compiler > V3.0.
 It is strong recommended to download and build Harbour from the fresh 3.2 source:
 git clone https://github.com/harbour/core.git harbour
 or to use latest binary package: https://sourceforge.net/projects/harbour-project/files/
 For this you need your C-Compiler used by Harbour in your OS search path.

 Then download latest source of LetoDBf, and move it to a new to be created directory:
 addons of the Harbour tree, e.g.; YouHarbourDirectory/addons/letodb
 This is not a must, but it will enable later to convenient use the letodb.hbc file for
 creation of you client applications.

 For Windows OS the letodb server can be compiled as Windows service or as daemon ( process ).
 For Linux it is possible as daemon ( recommended ) or in 'console mode' for debugging purpose.
 This is configured with macros __WIN_DAEMON__, __WIN_SERVICE__, __LINUX_DAEMON__, __CONSOLE__:

 letodb.hbp is ready configured for Windows and Linux daemon,
 letodbsvc.hbp is ready configured for use as Windows service.

 The server and the client library can be built als with support of the driver BMDBFCDX/BMDBFNTX.
 In this case, the basic RDD letodb server will be used instead DBFCDX/DBFNTX
 driver BMDBFCDX/BMDBFNTX, and supported the same functionality that BMDBF*.
 To build on this mode, in build scripts (letodb.hbp, rddleto.hbp for hbmk2
 and makefile.* for other compilers) it's need to set a macro __BM by un-commenting it.

 Default is to use LZ4 compression, this can be changed to classic ( much slower! ) zLib.


      2.1 recommended way : building letodb with hbmk2, for all C compilers

 Hbmk2 is a 'make' tool developed and provided by Viktor Szakats to the Harbour team.
 The command line syntax is:
      hbmk2 rddleto[.hbp]
      hbmk2 letodb[.hbp]

 To integrate LetoDbf client library into your Harbour environment as an 'addon':
      hbmk2 rddletoaddon[.hbp]
 To do this in Linux for an installed Harbour, you need root rights to do that, e.g.
      sudo hbmk2 rddletoaddon[.hbp]
 After successful build, you can compile your applications using Harbour .hbc file:
      hbmk2 your_application letodb.hbc.
 Look into the tests directory of LetoDBf for e.g. basic.prg, how such an application
 looks like. It will need very less additions, the rest of your application will stay
 as it was without LetoDBf usage.


      2.2 Borland Win32 C++ compiler

 An environment variable HB_PATH, which defines a path to the Harbour directory,
 must be set before compiling. This can be done, for example, by adding a line in a make_b32.bat:

          SET HB_PATH=c:\harbour

 Then run the make_b32.bat and you will get server executable file letodb.exe in a bin/
 directory and rdd library rddleto.lib in the lib/ directory.
 Tested to working with bcc v.5.5.1, but this compiler can NOT use LZ4 compression.


      2.3 MS Visual C compiler

 An environment variable HB_PATH, which defines a path to the Harbour
 directory, must be set before compiling. Or just change the value of HRB_DIR
 in the makefile.vc

 Then run the make_vc.bat and you will get server executable file letodb in a bin/
 directory and rdd library librddleto.lib in a lib/ directory.


      2.4.1 Mingw C compiler -- see point 2.1

      2.4.2 xHarbour compiler

 Support for xHarbour is broken, as newest capabilities of Harbour >= v3.0 are used.


      2.5 Additional Harbour functions support

 LetoDB use Harbour expression engine for evaluating index and filter expressions,
 and allows a call of the following functions:

      ABS, ALLTRIM, AT, CHR, CTOD, DATE, DAY, DELETED, DESCEND, DTOC, DTOS,
      EMPTY, I2BIN, L2BIN, LEFT, LEN, LOWER, LTRIM, MAX, MIN, MONTH, PAD, PADC,
      PADL, PADR, RAT, RECNO, RIGHT, ROUND, RTRIM, SPACE, STOD, STR, STRZERO,
      SUBSTR, REPLICATE, TIME, TRANSFORM, TRIM, UPPER, VAL, YEAR,
      hb_ATokens, hb_WildMatch

      Harbour extended time & date & timestamp functions:
      HB_DATETIME, HB_DTOT, HB_TTOD, HB_NTOT, HB_TTON, HB_CTOT, HB_TTOC, ;
      HB_TTOS, HB_STOT, HB_HOUR, HB_MINUTE, HB_SEC, HB_VALTOEXP, HB_ZCOMPRESS

 If you need else functions, it's necessary to add these in the source/server.prg module,
 done with:
      REQUEST <cFName1>[, ...]

 You can enable the full set of all basic Harbour commands to be available at server runtime.
 Herefore comment out 2 lines in source/server/server.prg.
 Or if you need the full set of the Harbour Cl*pper tools contrib, another two lines must be
 outcommented.


      2.6 Codepage support

  In the file: source/include/letocdp.ch you may adapt the list of available codepages.
 These will then be enabled/ loaded for a client connection,
 Each connection can use a different codepage, but you should not open the same DBF aka index
 files with different codepage. It is important, with which CP setting the index was created.


      3. Running and stopping server

 Start it, in default mode __WIN_DAEMON__ or both modes __LINUX_DAEMON__ and __CONSOLE__
      start /B letodb.exe           ( Windows )
      ./letodb                      ( Linux )

 To shutdown the server, run the executable with the 'stop' parameter:
      letodb.exe stop               ( Windows )
      ./letodb stop                 ( Linux )

 To reload letoudf.hrb module, run the same executable with the 'reload' parameter:
      letodb.exe reload             ( Windows )
      ./letodb reload               ( Linux )


 For use as "Windows service" server must be compiled with __WIN_SERVICE__ flag, see 2.

 The executable: "letodb.exe" and the config file "letodb.ini" must be copied into a directory,
 which is searched by Windows search path. This can be e.g. the "bin" directory of Harbour.
 To install and uninstall service, you need to be a user with administrative privileges.

 To install service, run letodb with 'install' parameter:
      letodb.exe install
 Verify letodbf.log for that the service was successful installed.
 With next Windows start, LetoDBf server shell be active, look again into log file.

 To start and stop LetoDBf service manually, run your Windows service manager.
 Search in the web, how to find the service manager in your Windows version.

 To uninstall service, run letodb with 'uninstall' parameter:
      letodb.exe uninstall
 and restart your Windows server.


      4. Server configuration

      4.1 letodb.ini

 You may provide a configuration file letodb.ini if you aren't satisfied with default values.
 This file is only read once with starting the server, after changes you have to restart the server.
 The file letodb.ini must be in the directory of the server executable [ or for Linux alternative in
 "/etc/letodb.ini" - the one in "/etc" will have precedence over one in the executable directory ].
 Currently following parameters exists ( default values are designated ).

      [MAIN]
      IP =                     -    IP address, where Letodb server listens for connections;
                                    If not set, all net interfaces ( all IP addresses ) available on the
                                    computer, are used. ! Leave it empty for Windows. !
      Server =                 -    IP address used by tools like management console to find the server.
                                    This can be, but must not be, the same as used for config option 'IP'.
      Port = 2812              -    Server port number, default is 2812 [ then 2813 used for second socket ]
                                    There are two! ports used by server, this and the following number.
                                    [ may read Internals: second socket .. ]
      DataPath =               -    PATH to a base directory on a server with your databases,
                                    may include also a drive letter for poor Windows systems
      LogPath =                -    PATH to a directory (with write access) for all log files,
                                    created mainly for debugging purpose.
                                    File letodbf.log for the main server will contain some info from settings
                                    at server starttime, plus info about new connected and disconneted clients
                                    if config option <DEBUG> level is greater zero [ 0 ].
                                    Each connection will get an own log file [ if DEBUG level is increased ].
      Default_Driver = CDX     -    default RDD to open files on server ( CDX/NTX )
                                    Can on demand changed by client with function: leto_DbDriver().
      Lock_Scheme = 0          -    If > 0, extended locking scheme will be used by server.
                                    Then DB_DBFLOCK_HB32 will be used for NTX/CDX;
                                    _or_ if set to 6, DB_DBFLOCK_CLIPPER2 for NTX, HB32 for other
                                    _or_ if set to 2, DB_DBFLOCK_COMIX for CDX, HB32 for other.
      Memo_Type =              -    memo type ( FPT/ DBT/ SMT ),
                                    Default: FPT for DBFCDX, DBT for DBFNTX;
      Memo_BSize =             -    for expert users !, this will change default memo blocksize
                                    for *NEW* created data tables.
      Lower_Path = 0           -    if 1, convert all paths and filenames to lower case;
      EnableFileFunc = 0       -    if 1, using of file functions ( leto_file(),
                                    leto_ferase(), leto_frename() is enabled;
      EnableAnyExt = 0         -    if 1, *creating* of data tables and indexes with
                                    any extention, other than standard ( dbf,cdx,ntx )
                                    is enabled;
      Pass_for_Login = 0       -    Lowest level of password verification: after login all is allowed to all
                                    if 1, user authentication is necessary to login to the server;
      Pass_for_Manage = 0      -    if 1, user authentication is necessary to use management functions,
                                    e.g. run the monitor console [ Leto_mggetinfo() ]
      Pass_for_Data = 0        -    if 1, user authentication is necessary to have write access to the data;
      Pass_File = "leto_users" -    the path and name of users info file;
      Share_Tables = 0         -    if 0 [ default ], LetoDB opens all tables in an exclusive mode,
                                    what leads to significant performance increase.
                                    If 1, tables are opened in the same mode [shared/exclusive] as client
                                    applications opened them, what allows LetoDB to work in coexistence with
                                    other applications [ non LetoDB users ] simultanous on the same DBF tables.
      No_Save_WA = 0           -    When this mode is set to '1', each dbUseArea() will cause a real file open
                                    operation and creating workareas on the server with same workarea settings
                                    as at client [ WA number, alias, filter conditions, relations ]
                                    ( in default mode '0' each file is opened only one time and have only one
                                    virtual workarea for all users -- no relations at server active ).
                                    When set to '1', it is also combined with "Share_Tables" setting, where
                                    "Share_Tables = 1" signalizes LetoDB that there will be 3rd party
                                    [non LetoDBf application] simultanous access to DBF tables.
                                    With "Share_Tables = 0" some internal performance increasing shortcuts
                                    can be done.
                                    Useful for UDF action at server, as the UDF function get same workarea
                                    environment as the client have.
      Cache_Records            -    The number of records to read into the client read cache,
                                    used for skipping etc. at client side without new requesting the server.
                                    Records are valid at client as long as the hotbuffer timeout.
                                    Default is 10, minimum is 1 (slow performance), good values are 10 - 50,
                                    theoretical ! maximum 65535. Adapt for performance in your environment.
                                    Can be set for specific tables and occasions with leto_SetSkipBuffer().
      Max_Vars_Number = 1000   -    Maximum number of shared variables
      Max_Var_Size = 65535     -    Maximim size of a text variable
      Trigger = <cFuncName>    -    Global function letodb RDDI_TRIGGER
      PendingTrigger = <cFuncName>- Global function letodb RDDI_PENDINGTRIGGER
      Tables_Max = 999         -    Number of *MAXIMUM* designated DBF tables handled by server,
                                    for server mode No_Save_WA == 0 this are physical DBF tables,
                                    for server mode No_Save_WA == 1 this are DBF tables opened by all users.
                                    This number can *not* be increased during runtime of server.
                                    Theoretically maximum value: 1000000, minimum: 10.
                                    Increase default value big enough to your needs,
                                    Example for No_Save_WA == 0: 2 * physical existing DBF
                                    Example for No_Save_WA == 1: Users_Max * physical existing DBF
                                    ( Maximum limit per one single user connection is about ~ 60000. )
      Users_Max = 99           -    Number of *MAXIMUM* designated users. do not set too low.
                                    This number can *not* be increased during runtime of server.
                                    Theoretically maximum value: 65534.
                                    Increase default value big enough to your needs, example two times as actually
                                    users, but do not exaggerate.
      Debug = 1                -    Debug level, default: 1 --> only minor information about login/ logout of
                                    connections is written into letodbf.log
                                    0 = none debugging messages -- notice that real errors are always logged.
                                    The greater the the value, the more information is written.
                                    A value >= 15 will include partly communication traffic to server,
                                    with a value > 20 the full communication protocol is logged.
                                    !! USE WITH CARE !!, the log files can get very quick VERY BIG.
                                    Set it for normal use to '1', '0' will avoid any debug feedback from
                                    server, then only errors are reported in these log files.
                                    ONLY increase value in case of problems, when core developer explicit
                                    ask for information to be found in the log files.
                                    It can be changed 'on the fly' for critcal sections with new
                                    RDDI_DEBUGLEVEL -- see 7.5
      HardCommit = 0           -    if 0, SET HARDCOMMIT OFF, this is now DEFAULT.
                                    It is recommended for UNSTABLE running server to set it
                                    to 1, which means that each change at data tables are immedeate
                                    written to harddrive bypassing the OS cache.
                                    Expect significant reduced performance with setting '0'.
      AutOrder = 0             -    SET AUTORDER setting, this will influence the SET( _SET_AUTOPEN ) setting.
                                    Default is to auto-open production index.
                                    Set to -1 will disable AutOpen at server and client.
                                    Setting this to >= 0 activates AutoOpen and also set the active order
                                    ( if possible ) to that value, when data tables are opened.
                                    Server default can be changed for each connection with:
                                    RddInfo( RDDI_AUTOORDER [, nNew ]
      Optimize = 1             -    _SET_OPTIMIZE setting
      ForceOpt = 0             -    _SET_FORCEOPT setting
      Allow_Udf = 0            -    security setting, DEFAULT is ! NOT ! to allow the use of
                                    loaded UserDefinedFunction for remote execution at server.
                                    With value 0 even a Leto_UDFExist() will deny to answer.
                                    Set to 1 to use UDF functionality on LetoDB server.
                                    0 will disable that possibility.
      TimeOut = -1             -    Connection timeout in seconds, -1 means infinite wait.
                                    This timeout determine, how long a write to network/ wait for requested workarea
                                    will wait to succeed, before the thread for the connection give up.
                                    If used: Zombie_Check, this value shell be shorter than that.
      Zombie_Check = 0         -    Time in seconds, that a client must be quiet ( no activity ), before
                                    an 'are you healthy' query (ping) is send from server, to verify it's not a
                                    dead/ unplugged connection. ! Application must be linked multi-thread ( '-mt' ) !,
                                    else these checks cannot be done.
                                    As 3 times for a given interval a check is done, a zombie can be 1/3 time
                                    longer 'dead', e.g. 60 ==> max. 80 seconds 'dead' before detected.
                                    Such connection will be shut down, opened files and locks are reseted.
                                    If set to 0 [ default ], these checks are diabled.

      It is possible to define [DATABASE] structure if you need to have a
 directory, where files are opened via other RDD:

      [DATABASE]
      DataPath =               -    (mandatory option)
      Driver = CDX             -    ( CDX/NTX )

      You can define as many [DATABASE] sections, as needed.

      In Windows environment the letodb.ini must be placed in a directory, from
 where server is started.
      In Linux the program looks for it in the directory from where the server
 is started and, if unsuccessfully, in the /etc directory.


      4.2 Authentication

 To turn authentication subsystem on, you need to set one of the following
 letodb.ini parameters to 1: Pass_for_Login, Pass_for_Manage, Pass_for_Data.
 Before, you need to create, at least, one user with admin rights, because when authentication
 subsystem works, only authenticated users with admin rights are able to add/change users
 and passwords.
      To add a user, you need to include a call of LETO_USERADD() in your client side
 program, for example:

      LETO_USERADD( "admin", "secret:", "YYY" )

 where "YYY" is a string, which gives rights to admin, manage and write access. You may
 also use the utils/manager/console.prg program to set or change authentication data.

 To connect to a server with an authentication data ( username and password ) you need to
 use LETO_CONNECT() function.


      5. Features of work with the letodb server

      5.1 Connecting to the server from client programs

 To be able to connect to the server you need to link the rddleto library
 to your aplication and add at start of a main source file ONE line:

      REQUEST LETO

 See for example given in tests/basic.prg.
 This will request the LetoDBf RDD and sets also the default to this ( RddSetDefault( "LETO" ).
 Further an additive idletask is activated, if you application is build with hbmk2 switch '-mt' for
 MultiThread support. This relates to the two port technic, and gives you performance advantages.

 There are two ways to open a DBF table at the server, the recommended is using leto_Connect().
 For both ways, you firstly set a so called 'root path' in config file for option:
 DataPath = [drive:]\path\to\data_diretory

      IF leto_Connect( "//192.168.5.22:2812/" ) < 0
         Alert( "Can't connect to server ..." )
         QUIT
      ENDIF

 With connection to the server, information about codepage is sent to server to be used for this
 connection for index keys etc. For further parameter of leto_Connect() see: 7.1

 After successfull connect to server, you use your ready application as done without LetoDBf.
 If your DBF names contain path elements, these are relative to the config option: <DataPath>
 at server. It is very recommended ! to set a <DataPath>, if no <DataPath> is set it will be the root
 directory [ Windows: of the drive with the server executable ].
 Example: DbUseArea( .T.,, "test\customer.dbf" ) will open DBF in:
      [drive:]\path\to\data_diretory\test\customer.dbf.
 Allowed is no drive letter in the realative path, and only one "..\" to step up one directory higher
 than the <DataPath>. Note that this path is equal to your SET DEFAULT TO setting.
 The filenames can have optional an OS dependent pre-leading '\' or '/', example: /mydbf.dbf

 The other way, not recommended because of not being portable, to open a DBF tables is the
 'on the fly' method by adding IP-address/ port number and relative path to Harbours 'USE' command,
 example:
       USE "//192.168.5.22:2812/mydir/test"
 This will open DBF table: [drive:]\path\to\data_directory\mydir\test.dbf


      5.2 Filters

 The filter is established usually: by the SET FILTER TO command or by a call
 DbSetFilter() function. The filter which can be executed on the server is called optimized.
 If the filter can't be executed on the server, it is not optimized. Such filter
 is slow as from the server all records, which are all the same requested then
 are filtered on the client.
 To set the optimized filter, it is necessary, that in logical expression for the filter were no variables
 or functions only known by the client. Also filter expressions containing ALIAS-> names for fields will be
 rejected as non optimized.
 With the help of leto_Var*() functions and UDF loadable functions in the UDF module, it is possible to turn
 a non-optimized filter into an optimized. For using leto_Var*() functions see notes about: leto_VarGetCached()
 To check, whether the filter is optimized, it is necessary to call the LETO_ISFLTOPTIM() function.

 SET( _SET_OPTIMIZE, .F. ) will disable any filter evalution at server, so every filter will become a
 non-optimized filter executed by client [ default is .T. ].
 Only with server mode: No_save_WA = 1 and then setting
 SET( _SET_FORCEOPT, .T. ) will enable all possible filter expressions at server, even those with ALIAS names
 in the expression string [ default is .F. ].


      5.3 Database driver

      If nowhere explicitely set, the default database driver for the server is DBFCDX.
 This default driver at server can be changed in the letodb.ini with setting of Default_Driver.
 Active driver for your connection, you can query and !SET! with:

       leto_DbDriver( [ <cNewDriver> ], [ <cMemoType> ], [ <nBlocksize> ] )
                                                               ==> aInfo

 This will return a 3 dimensional array, in the order of the parameters, so aInfo[ 1 ] is
 the active used driver. With no arguments the active setting is returned.
 You can change that at by using "DBFNTX", "DBFCDX", "DBFFPT", "DBFNSX" or "SIXDBF".
 Each driver can be aside their defaults combined with MemoType "DBT", "FPT" or "SMT".
 Further for expert ! users the blocksize used for the memofield can be changed:
 minimum is 32 Bytes, maximum 64KB == 65535 Bytes and always must be a multiple of 32 Bytes.
 !! Use leto_DbDriver() function before open or create new files !!
 This way you can even mix different drivers for a single connection. Sure you can not mix different
 drivers for the same database, so e.g. a DBFNTX table must be used for all connections as NTX type.


      5.4 Special Data Files in RAM

 LetoDB can create ( and share between connections ) data tables in RAM, not on harddrive.
 These HbMemIO called tables are especially useful for temporary tables. They are limited to
 available RAM at your server and only valid during one server run, all lost after LetoDB shutdown.

 Filenames for these special data tables optional can have optional a leading path seperator
 [ '/' or '\' ], follwed mandatory by a 'mem:' prefix. As example:
 "/mem:speeddata.dbf" would be a valid filename. These data tables work like 'real' data tables,
 so they are created using the active database driver ( see 5.3 ).
 When index orders are created, the bagname must also have this prefix: "mem:", else it will be
 created on harddrive.
 You also can combine a data table on harddisk with an ( temporary ) index in RAM.
 Or a data table in RAM with index on hard drive, but this combination makes not much sense ;-)
 These HbMemIO files have especially at slower harddrive some performance advantages.

 !! Don't forget to drop/ delete no more used hbMemIO files, else the server may quickly run out
 of available RAM. !!


      6. Variables management

      Letodb allows to manage variables, which are shared between applications, connected
 to the server. Variables are separated into groups and may be of logical, integer or
 character type. You can set a variable, get it, delete or increment/decrement
 ( numerical, of course ). All operations on variables are performed consecutively by
 one thread, so the variables may work as semaphores. See the functions list for a syntax
 of appropriate functions and tests/test_var.prg for a sample.
      Letodb.ini may contain lines to determine the maximum number of variables and a
 maximum size of a text variable.


      7. Functions list

      7.1 Connection management functions

      Below is a full ( at least, for the moment I write it ) list of functions,
 available for using in client applications with RDD LETO linked.

      LETO_CONNECT( cAddress, [ cUserName ], [ cPassword ],
                    [ nTimeOut ], [ nBufRefreshTime ], [ lZombieCheck ] )
                                                               ==> nConnection, -1 if failed
 <nTimeOut> defines, how log for an answer from server application will wait, in 0.001 seconds.
 This timeout value is valid for each request to the server, not only for the initial connect.
 Default is 120000 aka 2 minutes. '-1' means infinite wait. After that timespan, application will
 break with an error if no answer from server had been send.
 <nBufRefreshTime> defines the time interval in 0.01 second units. After this
 time is up, the records buffer will be refreshed, 100 by default (1 sec)
 lZombieCheck = .F. disable check for dead connection and also the second socket
 for faster communication with the server. Default is .T.
 If you use in letodb.ini configuration point: Pass_for_Data = 1, it is advised to
 disable lZombieCheck, aka to set it explicitely to .F.

      LETO_CONNECT_ERR( [ lAsText ])                           ==> nError [ cError ]
      LETO_DISCONNECT( [ cConnString | nConnection ] )         ==> nil

      Very dangerous functions removed, deprecated
      [  LETO_SETCURRENTCONNECTION( nConnection )              ==> nil
         LETO_GETCURRENTCONNECTION()                           ==> nConnection ]

      LETO_GETSERVERVERSION( [ lHarbourVersion ] )             ==> cVersion
 Returns version of LetoDBf server, with given .T. boolean parameter the version of Harbour at compile time.

      LETO_GETLOCALIP()                                        ==> IP address of client station
      LETO_ADDCDPTRANSLATE(cClientCdp, cServerCdp )            ==> nil
      LETO_PATH( [<cPath>], [cConnString | nConnection] )      ==> cOldPath


      7.2 Transaction functions


 !! During a transaction, after leto_BeginTransaction(), Recno() will be '0' after appending
    a blank record with e.g. DbAppend() !!

      LETO_BEGINTRANSACTION( [ nBlockLen ] )
 Parameter <nBlockLen> can be used for set memory allocation block size, default step-size is 1 KB.
 For big transaction it *minimal* might improve transaction speed at the client side.

 ! Important !: it is explicitly forbidden to mix RLock() and FLock() for one workarea. !

      LETO_ROLLBACK( [ lUnlockAll ] )                          ==> nil
      LETO_COMMITTRANSACTION( [ lUnlockAll ] )                 ==> lSuccess
 lUnlockAll default is true [ .T. ] and will automatic unlock all locks ( R-locks and F-locks ).
 Use lUnlockAll = .F. only in conjunction with F-locks during transaction, so these F-locks
 will remain active.


      LETO_INTRANSACTION()                                     ==> lTransactionActive


      7.3 Additional functions for current workarea

      LETO_COMMIT()
 This function can be used for current workarea instead of calls:  dbCommit(); dbUnlock()
 The client sends to the server 3 packages: for record updating on the server, for commit record
 and unlocking record. Leto_Commit sends only one package for all these operations.
 A file-lock is left active, only record locks are unlocked.

      LETO_DBEVAL( [ <cBlock> ], [ <cFor> ], [ <cWhile> ], [ nNext ], [ nRecord ], [ lRest ] )
                                                               ==>aResults
 ! The optional codeblocks: cBlock, cFor, cWhile must be given in literal form !
 This works alike Harbours' DbEval() command, but like an UDF directly at the server: this way only the
 resulting array is transferred over network, not all the records [ to be ] processed.
 Codeblock expressions may, but not mandatory, start/ end with '{' and '}' chars -- if these are missing,
 in front a '{||' and at end a '}' will be added.
 Codeblock receives as first argument a boolean, set to true if *first* valid record is processed, e.g.:
 "{ | lFirst | IIF( lFirst, DoThis(), DoThat() ), DoEver() }"
 ToDo : You can make e.g. sums in the codeblock by using THIS thread global var: xCB [ x means any content and CB codeblock ],
 "{ | lFirst | IIF( lFirst, xCB := 0, ), xCB += 1 }"

      LETO_SUM( <cFieldNames>|<cExpr>, [ cFilter ], [xScopeTop], [xScopeBottom] )
                                                               ==> nSumma if one field or expression passed, or
                                                               {nSumma1, nSumma2, ...} for several fields
 The first parameter of leto_sum is a comma separated list of fields or expressions,
 optional cFilter is a filter condition to be taken instead a possible active filter [ DbSetFilter() ],
 optional xScope[Top|Bottom] are scope values for an active index order,
 Example:
    leto_sum("NumField1,numField2,#", "CharField $ 'elch'", cScopeTop, cScopeBottom )
 returns an array with values of sum fields NumField1 and NumField2.

 If "#" symbol passed as field name, leto_sum returns a count of
 evaluated records, f.e:
 leto_sum("Sum1,Sum2,Sum1+Sum2,#", cFilter, cScopeTop, cScopeBottom)
                                                               ==> {nSum1, nSum2, nSum3, nCount}
 If only one field name or expression is passed, leto_sum() returns a numeric value


      LETO_GROUPBY( cGroup, <cFields>|<cExpr>, [cFilter], [xScopeTop], [xScopeBottom]) ==> aValues
                                                               {{xGroup1, nSumma1, nSumma2, ...}, ...}

 This function return two-dimensional array. The first element of each row is a value of <cGroup> field,
 elements from 2 - sum of comma separated fields or expressions, represented in <cFields>.
 If "#" symbol passed as field name in cFields, leto_groupby return a count of evaluated records in each group


      LETO_ISFLTOPTIM()                                        ==> lFilterOptimized
  To determine if an active filter in selected workarea is optimized [ aka executed only at server side ]
  or non-opimized [ server send all records to client, which then must decide itself for valid records. ]
  See 5.2 for more info.

      LETO_MEMOISEMPTY()                                       ==> lEmpty
  This is an optimzed function to very fast test, if a memofield of the current record is empty or not.
  The check will be done only at client side, so no network traffic to the server will occure.
  As it else would happen with a test like: EMPTY( FIELD->memofield ), for which the server will send the
  whole content of a memofield to the client, before client can decide if empty or not.

      dbInfo( DBI_BUFREFRESHTIME[, nNewVal])                   ==> nOldVal
  Setting new value for only one specific workarea: skip and seek buffers refresh time in 0.01 sec.
  If -1: connection setting is used. If 0 - buffers are used anyway.

      dbInfo( DBI_CLEARBUFFER )
  This command clears the skip buffer.


      7.4 Additional rdd functions

      leto_CloseAll( [ cConnString | nConnection ] )           ==> nil
 Close all workareas for specified or default connection

      leto_DbDriver( [ "cNewDriver" ], [ "cMemoType" ], [ nBlocksize ] )
                                                               ==> aInfo
 return an array with and in order of the three params.
 New values are optional, and then valid for the next ! opened/ created data table.
 Changing cMemoType will also change
 cNewDriver: "DBFNTX", "DBFCDX", "DBFNSX"
 cMemoType : "DBT", "FPT", "SMT"
 nBlockSize: default for DBT is 512, for FPT 64 and SMT 32 Bytes.
             minimum is 32 Bytes, maximum 65535 == 64 KB; new values as multiple of 32 Bytes.


      7.5 Setting client paramenter

      LETO_SETSKIPBUFFER( nSkip )                          ==> nSet (buffer statistic using)
 This buffer is intended for optimization of multiple calls of skip.
 This function set size of cached records in a skip buffer for current workarea.
 By default, the size of skip buffer is CACHE_RECORDS server config value. Skip buffer is bidirectional.
 Skip buffer is refreshed after BUFF_REFRESH_TIME ( default: 1 sec )
 If parameter <nSkip> is absent, function returns buffer statistic ( number of buffer hits )
 with given numeric value effective set size or 0 if no workarea was selected.

      LETO_SETSEEKBUFFER( nRecsInBuf )                     ==> 0
 ! DEPRECATED !

      LETO_SETFASTAPPEND( lFastAppend )                    ==> .F.
 ! DEPRECATED !
 because of ugly design problems. It is left as dummy function.
 If such functionality is really needed, encapsulate your request in a transaction.
 Or use: LetoCommit() after the record is appended, to spare an tiny unlock request to server.
 ( Note for experts: memo-fields need a record/ RecNo at server to be put, auto-incrementing fields also.
   In case the developer uses: DbAppend( [.T.] ), there are at least one record left locked at server, about
   the client have no information. Also functions like: RecCount() will become dangerous useless. )

      RddInfo( RDDI_REFRESHCOUNT, <lSet>,, [nConnection] )
 By default, the RDDI_REFRESHCOUNT flag is set to true.  If this flag is set, function: RecCount() retrieve
 amount of records from server.
 If not set, with common record data transmitted values are used and are maybe slightly out-timed.
 If other applications are appending records to the table, new value of records count won't be immediately received.
 If RDDI_REFRESHCOUNT flag is cleared, dbGoto(0) clears record buffer and set EOF and other flags instead of sending a
 server request.

      RddInfo( RDDI_DEBUGLEVEL [, nNewLevel ] )
 Reports [ and changes ] the debug level at server, responsible for amount of feedback in the log files.
 Use with care, log files will grow at a busy server in only some seconds MB stepwise ...
 For possible values look 4.1 :letodb.ini ...
 With <nNewLevel> this can be changed on the fly, no server restart is needed. This then applies to all 
 active and new server connections.


      7.6 File functions

 IMPORTANT: all file commands start at the DataPath, so <cFileName> is a relative path to
 the root path defined in letodb.ini with DataPath.  Only ONE ( 1 ) single ".." is allowed.
 Exception: filenames starting with "mem:" redirects into the RAM of server (virtual HbNetIO FS )

 Further the <cFileName> parameter of all file functions can contain a connection string
 to the letodb server in a format:
 //ip_address:port/%data_path%/[mem:]file_name.
 If the connection string is omitted, the currently active connection is used.

      Leto_File( cFileName )                                   ==> lFileExists
 Determine if file exist at the server, analog of File() function

      Leto_FCopy( cFilename, cFileNewName )                    ==> -1 if failed
 Copy a file at the server with a new name at server

      Leto_FErase( cFileName )                                 ==> -1 if failed
 Delete a file at the server.

      Leto_FRename( cFileName, cFileNewName )                  ==> -1 if failed
 Rename a file: <cFileName> ==> <cFileNewName>. <cFileNewName> should be without
 connection string.

      Leto_MemoRead( cFileName )                               ==> cStr
 Returns the contents of file at the server as character string, analog of
 MemoRead() function.

      Leto_MemoWrite( cFileName, cBuf )                        ==> lSuccess
 Writes a character string into a file at the server, analog of
 MemoWrit() function.

      Leto_MakeDir( cDirName )                                 ==> -1 if failed
 Creates a directory at the server

      Leto_FError()                                            ==> nError
 Returns an error code of last file function.

      Leto_FileRead( cFileName, nStart, nLen, @cBuf )          ==> -1 if failed
 Read a content of file at the server from <nStart> offset and max <nLen> length.
 Given nLen == 0 means the whole file.

      Leto_FileWrite( cFileName, nStart, cBuf )                ==> lSuccess
 Write <cBuf> character string to a file at the server from <nStart> offset and <nLen> length

      Leto_FileSize( cFileName )                               ==> -1 if failed
 Returns a length of file at the server

      Leto_FileAttr( cFileName [, cNewAttr] )                  ==> cAttr
 Get ( without given cNewAttr ) or set <cNewAttr> file attributes, where returned value
 <cAttr> are the active attributes ( after an optional change with <cNewAttr> )
 <cNewAttr> can contain at first place a "-", to revert the following attribute(s),
 e.g.; "-A" will remove the 'archive' attribute; "-" will remove all attributes.
 File attributes are only valid for FileSystem which support them !

      Leto_Directory( [ cDir ] [, cAttr] )                          ==> aDirectory
 Returns a content of directory at the server in the same format as Directory() function.
 With no given <cDir> the DataPath root directory is used.


      7.7 Management functions

      LETO_MGGETINFO()                                         ==> aInfo[17]
 This function returns parameters of current connection as 17-element array
 of char type values:
 aInfo[ 1]  - count of active users
 aInfo[ 2]  - max count of users
 aInfo[ 3]  - opened tables
 aInfo[ 4]  - max opened tables
 aInfo[ 5]  - 0 [ server up time moved into LETO_MGGETTIME()
 aInfo[ 6]  - count of operations
 aInfo[ 7]  - bytes sent
 aInfo[ 8]  - bytes read
 aInfo[ 9]  - opened indexes
 aInfo[10]  - max opened indexes
 aInfo[11]  - data path
 aInfo[12]  - server CPU(s) time in sum for all connections
 aInfo[13]  - ulWait
 aInfo[14]  - count of transactions
 aInfo[15]  - count successfully of transactions
 aInfo[16]  - 0 [ current memory used ] -- moved into LETO_MGSYSINFO()
 aInfo[17]  - 0 [ max memory used ] -- moved into LETO_MGSYSINFO()

      LETO_MGGETUSERS( [nTable] )                              ==> aInfo[x,5]
 Function returns two-dimensional array, each row is info about user:
 aInfo[i,1] - user number
 aInfo[i,2] - ip address
 aInfo[i,3] - net name of client
 aInfo[i,4] - program name
 aInfo[i,5] - timeout

      LETO_MGGETTABLES( [nUser] )                              ==> aInfo[x,3]
 aInfo[i,1] - filename of data table ( without root data path of server ).
 aInfo[i,2] - 0 if asked for all connections ( nUser < 0 ), else client workarea number.
 aInfo[i,3] - empty "" if asked for all users, else ALIAS name of client workarea .

      LETO_MGGETINDEX( [nUser], [nTable]                       ==> aInfo[x]
 array with filenames of opened index files, global for all connections ( nUser <  0 ),
 or only for a given user and then also only for a specific data table.
 Data table numbers must be retrieved before with above function.

      LETO_MGSYSINFO()                                         ==> aInfo[8]
 aInfo[1]   - free diskspace
 aInfo[2]   - data path of server
 aInfo[3]   - number of CPU cores of server
 aInfo[4]   - free available RAM of server
 aInfo[5]   - server mode ( 1 == No_Save_WA=1, 2 == Share_Tables=1, 3 == Share_Tables=0 )
 next following four items need special build of Harbour with memory statistics ( else all: 0 ),
 and are for debugging purpose only -- for constants see: hbmemory.ch
 aInfo[6]   - HB_MEM_USEDMAX     this and following three need special build Harbour
 aInfo[7]   - HB_MEM_STACKITEMS  with memory statistics-- for constants see: hbmemory.ch
 aInfo[8]   - HB_MEM_STACK
 aInfo[9]   - HB_MEM_STACK_TOP

      LETO_MGGETTIME()                                         ==> aInfo[3]
 Function returns array {<dDate>, <nSeconds>, <nServerUp>}:
 aInfo[1]   - server dDate;
 aInfo[2]   - server time in nSeconds after midnight.
 aInfo[3]   - server UP time in nSeconds
 Convert first tho values to a datetime variable (Harbour):
 hb_DTOT( aDateTime[1], aDateTime[2] )

      LETO_MGKILL( nConnection | IPAddress )                   ==> nConnectionClosed
 Kill user number given by <nConnection> or cIPAddress,
 returns number of killed connection or -1 in case of not found

      LETO_LOCKCONN( lOnOff )                                  ==> lSuccess
 After leto_lockconn( .t. ) request new connections are blocked by server, until
 leto_lockconn( .f. ) called

      LETO_LOCKLOCK( [ lOnOff ] [, nSecs ] )                   ==> lSuccess
 This function wait until any updates/ locks are closed, then commit all changes.
 Afterwards to lock server from any updates from clients, by e.g. to lock a record.
 It returns True, if server is succesfull locked, no user with open locks active.
 Default timeout are 30 seconds.
 Without <lOnOff> you get answer if server is locked() for you [ false for the one who locked server ].

      LETO_TOGGLEZIP( [ nCompessLevel [, cPassword ] ] )       ==> nValueBefore
 This function on demand [de-]activates network traffic compression between server and client,
 where nCompessLevel is:
 -1: == compression off, regularly optimized data traffic,
 [ 0 = none compression, used only by developer for debugging purpose ]
 1 : LZ4 [ or zlib ] compression level.
 If you have compiled LetoDBf server and client with LZ4 highspeed realtime compression [default],
 then 1 is recommended value for compression.
 If LZ4 is disabled during LeoDBf build, then zlib compression is used by Harbour. Also here '1' is
 recommended value, but zLib knows of values 1 to 9.
 With nCompression >= 0, also a cPassword can be given for addtional traffic encryption,
 Initial connect to server ( e.g. LETO_CONNECT() ) is ever done in regularly traffic [ -1 ] mode.
 Without any function param, the active compression setting is returned without changing it.


      7.8 User account management functions

      LETO_USERADD( cUserName, cPass [, cRights ] )            ==> lSuccess
      LETO_USERDELETE( cUserName )                             ==> lSuccess
  New function ;-), now a user can be deleted
      LETO_USERPASSWD( cUserName, cPass )                      ==> lSuccess
      LETO_USERRIGHTS( cUserName, cRights )                    ==> lSuccess

      LETO_USERFLUSH()                                         ==> lSuccess
  Writes the changes in memory into file on storage, will be done else when server shuts down.

      LETO_USERGETRIGHTS()                                     ==> cRights
  Return the rights of the active user.

  Generally the user who want to change/ add/ delete user properties must have admin rights in case
  in letodb.ini any Pass_For_* is set to '1'.
  cRights are three letters: Y == allow, N == deny, in sequence for:
  admin == change/ add users; manage == console user; write == allow to change data in DBF tables.

  The whole password file on storage is now encrypted, and also after loading the content during
  server start into memory, the passwords are kept encrypted. They will be de-crypted only
  'on the fly' to countercheck the password for the one user who asks for access.


      7.9 Server variable management functions

      LETO_VARSET( cGroupName, cVarName, xValue [, nFlags [, @xRetValue]] )
                                                               ==> lSuccess

 This function assign value <xValue> to variable <cVarName> from group <cGroupName>.
 xValue can be boolean, integer, decimal [NEW!] or string.
 Group- and Var- name are NOT trimmed of white spaces, char: ';' is an invalid char.
 Optional parameter <nFlags> defines variable create mode:
 LETO_VCREAT    - if variable doesn't exist, it's created;
 LETO_VOWN      - own user variable (deleted after user disconnect);
 LETO_VDENYWR   - write deny for other users;
 LETO_VDENYRD   - read deny for other users;
 LETO_VPREVIOUS - return previous value to <xRetValue> parameter.

      LETO_VARGET( cGroupName, cVarName )                      ==> xValue

 Function return value of variable <cVarName> from group <cGroupName>

      LETO_VARINCR( cGroupName, cVarName, nFlags )             ==> nValue
      LETO_VARDECR( cGroupName, cVarName, nFlags )             ==> nValue

 Function increments /decrements integer value of variable <cVarName> of group <cGroupName>.
 ! Only allowed for integer [ INT() ] variables !
 Remark that e.g. a result of a division: 'x / y' is ever a decimal value.
 So in doubt use: INT( value ) when you create such a variable for [in|de]crementing.

      LETO_VARDEL( cGroupName[, cVarName ] )                   ==> lSuccess

 Without <cVarName>, all variable members of <cGroupName> and the group itself is deleted.
 With given <cVarName>, variable <cVarName> is deleted from group <cGroupName>.

      LETO_VARGETLIST( [cGroupName [, nMaxLen]] )              ==> aList

 Function return two-dimensional array with variables: { {<cVarName>, <value>}, ...}


 special:
      LETO_VARGETCACHED()                                      ==> xValue [NIL]

 It returns the *last changed* LETO_VAR[SET|INCR|DECR]() variable *value*.
 ! To be used with care !
 When one *connection/user* set other variable, it will change the last cached value.
 If then used in a filter condition, and type of value [ string, numeric, .. ] changed,
 the filter gets invalid !
 If none variable was set before by this connection, NIL value is returned.
 Leto_VarGet[List]() have no influence, so you can query for other variables.
 Leto_VarDel() done by *same user* who set variable cache, will clear the value to NIL.
 Cached value is personal for one user/thread, so *others* can simultanous use same variable,
 or even delete it, without interfering the personally cached value.

 The tricky idea is following: first you create at client application a variable with:
    'leto_VarSet( "MY_GROUP", "MY_VAR", xValue, LETO_VCREAT )'
 then you define a filter condition for your workarea, example for a numeric:
    DbSetFilter( NIL, "field_var < leto_VarGetCached()" )
 Avoid 'ALIAS->' names in filter condition for mode: No_Save_WA=0: they are invalid.
 The only param what really atters is the filter text value, codeblock is optional.
 Function names requested ONLY in filter condition must be explicitely requested by a:
 REQUEST statement to be linked to your application.

 This gives the filter condition at client very! fast access to the value WITHOUT asking the
 server, also the server have very fast access to the cached last value.
 The connection which set filter can interactive change the content of this variable
 with a new leto_Var[Set|Incr|Decr]() to update the filter condition, as that will set
 a new cached value.
 Tip: if there are millions of records, and filter condition reduces them to just a few,
 it may increase performance lowering default amount of returned records with
 leto_setSkipBuffer() if not as many [ default: 10 ]records with one DbSkip() request needed.


      7.10 Calling udf-functions on the server

      LETO_UDF( cSeverFunc, xParam1, ... )                     ==> xResult
 This function is called from client application. The string <cServerFunc> can
 optional contains a server connection string, minimum is udf function name:
 [ //ip_address:port/ ]funcname
 A <funcname> function should be defined on the letodb server.
 The first parameter of udf function is nUserStru.
 Udf function can return result (any type) to client.
 Examples of udf-functions are in the tests/letoudf.prg

      LETO_RPC( cSeverFunc, xParam1, ... )                     ==> NIL !
 ! Use with care !, it needs very well designed UDF functions.
 Function name is up now limited to 20 char length.
 It is same a bit similar as for LETO_UDF, but the function started will have NO workareas
 opened: it is like a fresh connection, independent from the one who started it.
 After such job is started, no wait for the result will occure and and immedeate return to
 personal common work. You can start multiple jobs this way.
 This needs special care not to block others in the network from accessing their workareas.
 It is advised to be used in server mode 3: No_save_Wa = 1.

 This is higly interesting for tasks, where you need no feedback, e.g. some 'cleaning'
 or calculating jobs.
 Sure you also can write a result of something to a DBF and retrieve that later.
 But when it starts, it have no workarea active, like a new connection to server.
 Such UDF functions should be *VERY* careful designed, as they can NOT be stopped by
 the connection which started them, so better to test them intensive forehand with
 LETO_UDF ...
 Such 'headless' UDF at server can only be stopped by the management console, if the
 running UDF is designed to repeatedly check for: leto_UDFmustQuit().
 If it ignores that, it is unstoppable until server shutdown ...

      LETO_UDFEXIST( cSeverFunc )                              ==> lExist
 leto_udfExist check the existance of udf-function at the letodb server.
 <cSeverFunc> parameter is the same as for leto_udf().

      LETO_PARSEREC( cRecBuf )
 ! deprecated - removed !

      LETO_PARSERECORDS( cRecBuf )
 ! deprecated - removed !
 see new LETO_DBEVAL() as powerful alternative, or sample of UDF_dbEval() in tests/letoudf.prg


      7.11 Functions for bitmap filters

 If letodb compiled with rdd BMDBFCDX/BMDBFNTX, then there is support
 the following functions:

      LBM_DbGetFilterArray()                                   ==> aFilterRec
      LBM_DbSetFilterArray( aFilterRec )                       ==> nil
      LBM_DbSetFilterArrayAdd( aFilterRec )                    ==> nil
      LBM_DbSetFilterArrayDel( aFilterRec )                    ==> nil

 Purpose and the parameters of these functions is the same as for the
 corresponding functions BM_*.

      LBM_DbSetFilter( [<xScope>], [<xScopeBottom>], [<cFilter>] )
                                                               ==> nil
 This function set bitmap filter by current index order and for condition,
 defined in <xScope>, <xScopeBottom>, <cFilter> parameters.
 The current record after LBM_DbSetFilter() is the first record satisfying
 filter condition.


      7.12 Miscellaneous Functions

      leto_Hash( cText )                                       ==> nHashValue
 Returns a 32bit hash value of the given <cText>, useful to fast search for a string.
 Depending on how LetoDBF is compiled, MurMur3 hashing algorithm [default] or a homebrew
 algorithm is used. There may be rare collisions, means that it is possible that two <cText>
 resulting into same <nHashValue>. To be at safe side, ever compare the text char-by-char,
 after a valid hash-value is found. 


      8. Management utility

 There are two management utilities, GUI and console, the sources are in utils/manage directory.

 Windows only GUI utility, manage.prg, is made with the HwGUI library. If you have HwGUI,
 just write in the line 'set HWGUI_INSTALL=' in utils/manage/bld.bat a path
 to your HwGUI directory and run the bld.bat, it will build manage.exe for you.

 For those, who doesn't use HwGUI, there is a clored ! console mode utility:
 console.prg. Build a console.exe with a make/bat files, which you use to build
 Harbour single-file programs, you just need to add rddleto.lib to the libraries
 list. Run the console.exe with a server name or ip and port as a parameter:

      console.exe server_name:nPort
      console.exe ip_address:nPort

 The server_name and ip_address in a command line must be without leading
 slashes ( '//' ), because Clipper/Harbour interprets them in a special way.


      9. Server-side functions

 !!! CHANGED: nUserStru deprecated !!!
 The first parameter for such UDF functions formerly was numeric type, it is now obsolete.
 As result the places for your params in your older defined functions rise up one place in order.

 These functions can be run from the client by function leto_udf,
 and includes also functions defined in the file letoudf.hrb, loaded with server start.

      leto_SetEnv( xTopScope, xScopeBottom, xOrder, cFilter, lDeleted )
      leto_ClearEnv( xTopScope, xScopeBottom, xOrder, cFilter )
 These functions save with setting and later restore possible changed conditions by your UDF.
 Just fill in all param that are needed to be set, and call leto_ClearEnv() with the same params set.
 Scope with filter example: leto_SetEnv( "1000", "2000" ) => leto_ClearEnv( "1000", "2000" )

      leto_Alias( cClientAlias )                               ==> cServerAlias
 This function return the ALIAS name used at server for a given client side alias <cClientAlias>.
 Side effect is, if ALIAS name is valid, it will change the active selected workarea at server.
 The returned server ALIAS then can be user in usual RDD-operations.
 This function is mainly needed for mode 'No_Save_WA=0', here to use additional different workareas
 than the one which was actve when an UDF was initalially called.
 In server mode: 'No_Save_WA=0', ALIAS names at server and ALIAS name at client are different.
 All workareas used in an UDF are not available for other connections as long as the UDF is working.
 In server mode: 'No_Save_WA=1', ALIAS names at server and client side are the same.
 Also in this mode there is no exclusive restriction of workarea use only for the UDF connection,
 and using leto_Alias() gets obsolete.

      leto_RecLock( [ nRecord ], [ nSecs ] )                   ==> lSuccess
  leto_Reclock() function locks record with <nRecord> number, or the current record for data change
  access.
  <nSecs> param is only available in server file open mode: No_save_WA = 1.
  With given optional <nSecs> [ can be decimal: 1.5 ] it will wait for success if not
  immediate succesfull.

      leto_RecUnLock( [ nRecord ] )                            ==> Nil
  leto_RecUnlock function unlocks a locked record with <nRecord> number, or the current record.

      leto_RecLockList( aRecNo )                               ==> lSuccess
  leto_ReclockList function locked records with number in the <aRecNo> array.
  If any record isn't locked, all records are unlocked, and function returns
  .F. result.
  This function can be used at the server from letoodf.prg module, or from
  client by a call leto_UDF( "leto_RecLockList", aRecNo ).

      leto_TableLock( [ nSecs ] )                              ==> lSuccess
      leto_TableUnLock()
  [ deprecated: no more <nFlags> param available ]
  This will lock the whole DBF table, not only a single record, for data change access.
  <nSecs> param is only available in server file open mode: No_save_WA = 1.
  With given optional <nSecs> [ can be decimal: 1.5 ] it will wait for success if not
  immediate succesfull.

      leto_SelectArea( nAreaId )                               ==> lSuccess

      leto_Select( [ ncClientAlias ] )                         ==> nWorkareaID
 Selects to the workarea given by workarea ID oder ALIAS ( if valid )

      leto_AreaID( [ cAlias ] )                                ==> nAreaId
 Function return internal workarea-ID of current/ by ALIAS name given workarea,
 0 if not found

 [ Following four functions have been renamed for LetoDBf -- to be the same like in Harbour,
   but with prefix: 'leto_' ]

      leto_DbUseArea( [ cDriver ], cFileName, [cAlias, lShared, lReadOnly, cdp ] )
                                                               ==> nAreaId
 Parmeters order like DbUseArea(), EXCEPT the omitted first: table is ever in a new workarea,
 you can NOT preselect the Workarea-ID.
 If not given, cAlias is created from filename, cDriver is the last used setting,
 lShared is the default of _SET_EXCLUSIVE from LetoDBf client!,
 lReadonly is default .F. and codepage is settings used at client side.

      leto_DbCreate( cFilename, aStruct, [ cRDD ] , [ lKeepOpen ], [ cAlias ], [ cCodePage ] )
                                                               ==> lSuccess
 Creates a new database, some param are optional --  cFilename can be prefixed with 'mem:'.
 cRDD can be one of: DBFNTX, DBFCDX, DBFFPT, SIXCDX, DBFNSX -- if not given the last used RDD.
 lKeepOpen default is close file after create.
 cAlias is only needed, if the fresh created table should stay open.
 cCodepage will be the last used -- best to leave ever empty, especially if you not exactly know
 the cause why you want to set it.

      leto_OrdListAdd( cBagName [, ncAreaID ] )      ==> lSuccess
 It is recommended, to use the second param ncAreaID, best as cALIAS or as workarea number to on the
 safe side. LetoDBf will add all index keys in one [ multi tag ] index file at once.
 nAreaID is optional, if not given the actual selected workarea is used.

      leto_OrdCreate( [ ncAreaID ], cBagName, cKey, cTagName,
                      lUnique, cFor, cWhile, lAll, nRecNo,
                      nNext, lRest, lDesc, lCustom, lAdditive )
                                                               ==> lSuccess
 It is recommended, to use the first param ncAreaID, best as cALIAS or as workarea number.
 But the ID at first place can also be omitted, then the order will be created for that WA.
 cBagName is in any case the second param, cKey the index expression as string.
 For the other values see Harbour documentation for OrdCondSet().

      leto_DbCloseArea( [ ncAreaID ] )                         ==> lSuccess
 Close the active or by numeric or by string ALIAS given workarea.

      leto_DbEval( cbBlock, cbFor, cbWhile, nNext, nRecord, lRest )
                                                               ==> aResults

 This UDF function is same kind as DbEval() of Harbour. The three codeblocks may be given
 instead as a codeBlock within an UDF function or as string parameters when called 'directly'
 from a client per leto_udf( "leto_DbEval", "cBlock", ... ).
 If no cbBlock is given ( nor as CB neither as not empty string ), you get the raw content of a whole
 record in LetoDBf format, else the result of evaluated cbBlock for each record converted to string.
 This is a very migthy functionality, so should be used with some care.
 cbFor means a condition to be valid, else the record is unprocessed skipped over [ but counts for nNext ].
 cbWhile means an expression to stop when it results false ( .F. ). If empty, default is: '.NOT. EOF()'.
 Both cbFor and cbWhile must return logical ( boolean ) values.
 If nNext is 0, all records down to EOF() [ or cbWhileare processed, else the amount as given.
 lRest == true ( .T. ) starts from actual record, lRest == false ( .F. ) from topmost, first record.

 The functions leto_DbUseArea, leto_OrdListAdd, leto_DbCreate(), leto_OrdCreate, leto_DbCloseArea,
 leto_RecLock, is intended for using in udf-functions instead
 of rdd functions: dbUseArea, OrdListAdd, OrdCreate, dbCloseArea, RLock, dbUnlock

      leto_UDFMustQuit()                                       ==> lTrue
 If the console monitor quits a UDF or RPC thread on server, it sets an internal variable for this
 thread. This functions reports about that, then it shell end.
 Example: DO WHILE .NOT. leto_UDFMustQuit(); do something; ENDDO; RETURN
 Especially useful for threads started with Leto_RPC().

 Following functions same as for LetoDBf client, now also without nUserStru.
 Note: xValue will be 'NIL' in case of not found cGroupName/ cVarName.

      LETO_VARSET( cGroupName, cVarName, xValue[, nFlags )     ==> xValue
      LETO_VARGET( cGroupName, cVarName )                      ==> xValue
      LETO_VARINCR( cGroupName, cVarName )                     ==> nValue
      LETO_VARDECR( cGroupName, cVarName )                     ==> nValue
      LETO_VARDEL( cGroupName, cVarName )                      ==> lSuccess
      LETO_VARGETLIST( [cGroupName, [lValue]] )                ==> aList

 special:
      LETO_VARGETCACHED()                                      ==> xValue

 ! see explanations for purpose at client side function leto_VarGetCached() !



      A. Some Internals

      Some explanations, what happens 'under the hood'.
 Server is developed Multi-Threaded, what is 'a bit' compareable of running multiple instances
 or many programs on a computer. This will use all the CPUs of your server and span the load over
 them.
 There runs a main thread, responsible for incoming connection request, which will start for every
 connection to a user a new thread. All the communication to one connection is then done in this new
 thread. During server start are also two further threads started: one is like the main thread
 responsible for handling the initially communication sequence for the second socket (*) for a connection.
 This only will happen for multithreaded linked applications by setting hbmk2 option; '-mt'
 If enabled, one other thread periodically checks for dead connections, and will close them in that case.
 This can be adjusted by setting the timespan for check to zero [ default is: Zombie_Check = 0 ]
 in letodb.ini.

 (*) Second socket: this second port to the server is used also for mentioned 'dead connection* check.
 And used for a group of requests by client to the server: these requests have common, that you regularly
 only receive an 'ACK' == 'succesful done' from server. This 'ACK' will NOT be send, and only in case
 of an error a negative 'NO-ACK' will be send over this second port, so i called it the 'error socket'.
 This technic drastically speeds up e.g. updating records at server, as the client must not wait
 for the positive 'ACK', can immediate proceed and also network traffic is avoided.
 But the client will not miss such a negative 'NO-ACK': in such case a runtime error message will
 pop up 'delayed' with the known Harbour error handling system at client/ user side.

 Requesting and detaching workareas: many other server, also HbNetIO, will open a DBF table again for each
 connection after user command: DbUseArea(). Detaching an opened workarea means, it is at server side 'closed'
 for this connection, and put into a pool of 'detached workareas'.
 If the connection needs the workarea again for next action, it requests it out of the pool of detached
 workareas. Immediate after each action, the WA is detached again.
 If another connection want to open same DBF table, it instead requests it out of the pool of detached WA,
 and detaches it again after action is done.
 In this kind, a DBF table is opened only one time by server, and by detaching/ requesting then exchanged
 between multiple connections. This have significant performance advantage for below explained default mode 1.
 Disadvanttage is: as long as one connection/ UDF needs this WA for action, no other connection can use it.
 Further it is at only one single Workarea for one connection active: this is important for UDF what need more
 than one WA.

 File open modes: LetoDBf knows three (4) modes, choose the appropiate for you needs.
 #1# Share_Tables = 0, No_Save_WA = 0 [ default if not explicitely set ]
 The server exclusive use the DBF files, so no third party software ( non LetoDBf user ) can access
 DBF tables opend by LetoDBf server. This is the fastest server mode.
 Workareas are exchanged between connections using above explained detach/ request technic.
 #2# Share_Tables = 1, No_Save_WA = 0
 Second mode is equal, except the DBFs are in this mode opened according the first connection request.
 If first user of a DBF table open it with shared attribute for DbUseArea(), also other connections can
 use it. If first connection open DBF table exclusive, no other can use that workarea simultanous.
 Open a DBF table in shared mode gives third party / non LetoDBf users the possibility to work simultanous
 with DBF tables opened by LetoDBf server. This mode is a bit slower, but when concurency access is needed
 the way to go.
 #3# No_Save_WA = 1, Share_Tables option will indicate 3rd party software is active along the server.
 The workareas are NOT exchanged between connections using detach/ request technic.
 At server side each DBF is opened in exact the same workarea-ID and ALIAS name as at client side, and
 for every next connection once again -- further shared or exclusive, like client requested the DBF table.
 In this mode, all relations and filters at client side are active at server side.
 This will enable index- or filter- expressions on FIELDs of a relationed workarea.
 This mode is generally the slowest mode, but with fast filter action. It have great advantages for long running
 UDF functions and may have performance advantages for very many simultanous users accessing the same table,
 as in this mode one connection must not wait for a detached workarea from other thread.

 From POV of performance, mode #1# is the fastest, #3# in between, slowest is #2#.
 But expect only some % performance difference between the modes, as the most limiting factor is the TCP/IP network
 itself: the count of data-packages in a timespan is limited, and each request to server and response from it, is
 a whole data package, often filled with much less content as a single package can take (~ 1500 bytes). 

 ...


      B. Links, to 3rd party etc.

 MurMurHash version 3, 32bit hashing results, public domain, taken out of:
      https://github.com/aappleby/smhasher
 LetoDBF distributes only PMurHash.[c|h]

 LZ4 compression algorithm, BSD license, extreme fast compression and even more fast de-compression,
 used for on demand realtime network traffic:
      https://github.com/lz4/lz4
 LetoDBF distributes only the 'lib' directory with BSD license of that contrib.



 whish all possible fun !
