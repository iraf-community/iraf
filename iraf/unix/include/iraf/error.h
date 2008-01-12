/* Error handling.
 */
#ifndef	D_error
#define	D_error

#include <iraf/spp.h>

#define	EA_FATAL	1
#define	EA_ERROR	2
#define	EA_WARN		3
#define	EA_RESTART	(-99)

#define	SYS_XACV	501
#define	SYS_XARITH	502
#define	SYS_XINT	503

/* ../../../sys/libc/cerract.c */
extern void c_erract ( int );
/* ../../../sys/libc/cerror.c */
extern void c_error ( int, const char * );
/* ../../../sys/libc/cerrget.c */
extern int c_errget ( char *, iraf_size_t );
/* ../../../sys/libc/cerrcode.c */
extern int c_errcode( void );

#endif
