#ifndef SPLICE_H

#define SPLICE_H

#if defined( __i386__ )
   #define __NR_sys_splice		313
#elif defined( __x86_64__ )
   #define __NR_sys_splice		275
#elif defined( __powerpc__ ) || defined( __powerpc64__ )
   #define __NR_sys_splice		283
#elif defined( __ia64__ )
   #define __NR_sys_splice		1297
#endif

#define SPLICE_F_MOVE	( 0x01 )

static inline int ssplice( int fdin, loff_t *off_in, int fdout, loff_t *off_out,
                           size_t len, unsigned int flags )
{
	return syscall( __NR_sys_splice, fdin, off_in, fdout, off_out, len, flags );
}

#endif
