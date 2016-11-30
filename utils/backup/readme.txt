     letobackup - the utility intended for creation of a backup of a DB in any
moment. Before archive creation attempt of locking of the server is carried out.
If the server letodb is locked, operations of updating of a DB aren't supposed:
Record or file locking, data writing, addition of records, creation of
indexes and tables. Locking attempt consists of waiting of all
operations of updating of the data. If during the specified timeout not all
such operations have come to the end, the server isn't locked.
The server is locked for the period of creation of a backup of a DB. After end
of copying of a DB the server will be unlocked. Server locking is necessary
for maintenance integrity of a DB. Probably to specify a mode, when before
creation of a copy of a DB attempt server locking it is not carried out,
and a mode when the DB copy is created and in that case when server locking
isn't has gone right. In this case integrity of the data will not be guaranteed.
     letobackup it should be started on the server. At the first start of the utility
all DB is copied, at the subsequent starts - only the changed data.
After copying of the data it is possible to start the external archiver for
archive creation DB.

     Parameters letobackup which are in a file letbbackup.ini desctibed below:

# the Line of connection with the server

Server =//127.0.0.1:2812

# the full path to a database on the server. This parameter should be the same as
# DataPath in a file letodb.ini. For OS Windows return should be specified
# a slash "\"

DataPath = c:\database

# the path of a database for backup. The path is set concerning the path,
# specified in DataPath. If all DB it is necessary to specify value is copied
# "/". In a path the direct slash "/" should be used.

DataBase =/data1/

# the pathe in which the DB backup is created. The direct slash "\" should be used.

Backup = c:\backup\

# Masks of files for copying. Masks are specified through a comma. Are copied
# only the files satisfying set masks.

Mask = *.dbf, *.fpt, *.dbt

# If value Lock=1 is specified, before archive creation attempt is carried out
# server locking

Lock = 1

# Time in seconds, during which archive is carried out locking attempt
# server, by default - 30 seconds

Seconds = 30

# If value Wait=1 is specified, the successful is necessary for backup
# server locking. If value of parameter isn't specified, the backup will be
# created anyway

Wait = 1

# the command line for start of the external archiver which is started after
# backup. Symbols %dt% in line will be replaced with current date,
# and symbols %tm% - for current time.

ArcCmd = "C:\Program files\7-zip\7z.exe" a-r c:\arc\data%dt%_%tm% c:\backup\*.*
