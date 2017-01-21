/* LetoDBf client need to track changing SET DELETED to clear a possible skipbuffer */

#ifndef LETO_STD_CH_
#define LETO_STD_CH_

#command SET SOFTSEEK <x:ON,OFF,&>   => Leto_Set( _SET_SOFTSEEK, <(x)> )
#command SET SOFTSEEK (<x>)          => Leto_Set( _SET_SOFTSEEK, <x> )
#command SET DELETED <x:ON,OFF,&>    => Leto_Set( _SET_DELETED, <(x)> )
#command SET DELETED (<x>)           => Leto_Set( _SET_DELETED, <x> )
#command SET AUTORDER TO             => Leto_Set( _SET_AUTORDER, 0 )
#command SET AUTORDER TO <x>         => Leto_Set( _SET_AUTORDER, <x> )
#command SET AUTOPEN <x:ON,OFF,&>    => Leto_Set( _SET_AUTOPEN, <(x)> )
#command SET AUTOPEN (<x>)           => Leto_Set( _SET_AUTOPEN, <x> )

#endif  /* LETO_STD_CH_ */
