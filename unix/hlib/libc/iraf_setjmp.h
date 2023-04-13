/* SETJMP, LONGJMP -- Non local goto.  Requires libc.h and knames.h.
 * Note that jmp_buf must be at least one int larger than necessary to
 */
#ifndef	D_iraf_setjmp_h
#define	D_iraf_setjmp_h

#include "iraf_libc.h"
#include "iraf_knames.h"
#include "iraf_kproto.h"

typedef	XINT	jmp_buf[LEN_JUMPBUF];
static	XINT	u_jmpstat;

#define	setjmp(e)	(ZSVJMP((e),&u_jmpstat),u_jmpstat)
#define	longjmp(e,v)	(u_jmpstat=(v),ZDOJMP((e),&u_jmpstat))

#endif /* D_iraf_setjmp_h */
