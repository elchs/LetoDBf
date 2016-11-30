

                              __         __        ____  ____  __
                             / /   ___  / /_____  / __ \/ __ )/ _|
                            / /   / _ \/ __/ __ \/ / / / __  | |_
                           / /___/  __/ /_/ /_/ / /_/ / /_/ /|  _|
                          /_____/\___/\__/\____/_____/_____/ |_|


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


 Check Readme.txt for detailed help. Wish much fun.
 elch