/* LetoDBf client need to track changing SET DELETED to clear a possible skipbuffer */

#ifndef LETO_STD_CH_
#define LETO_STD_CH_

#command SET DELETED <x:ON,OFF,&>      => Leto_SetGet( _SET_DELETED, <(x)> )
#command SET DELETED (<x>)             => Leto_SetGet( _SET_DELETED, <x> )

#endif  /* LETO_STD_CH_ */
