/* SETJMP, LONGJMP -- Non local goto.  Requires libc.h and knames.h.
 * Note that jmp_buf must be at least one int larger than necessary to
 */
#ifndef D_libc
#ifndef import_libc
#include "libc.h"
#endif
#ifndef import_knames
#include "knames.h"
#endif
#endif

typedef	int	jmp_buf[LEN_JUMPBUF];
static	XINT	u_jmpstat;

#define	setjmp(e)	(ZSVJMP((e),&u_jmpstat),u_jmpstat)
#define	longjmp(e,v)	(u_jmpstat=(v),ZDOJMP((e),&u_jmpstat))

/* The following is necessary to prevent to prevent the optimizer from
 * doing unwise things with setjmp on a Sun-4.
 */
extern	int zsvjmp_();
#pragma unknown_control_flow(zsvjmp_)

#define	D_setjmp
