/* SETJMP, LONGJMP -- Non local goto.  Requires libc.h and knames.h.
 * Note that jmp_buf must be at least one int larger than necessary to
 */
#ifndef D_setjmp
#define	D_setjmp

#include <iraf/spp.h>
#include <iraf/knames.h>

typedef	XPOINTER	jmp_buf[LEN_JUMPBUF];
extern	XINT	u_jmpstat;

#ifndef NOLIBCNAMES
#define	D_setjmp_libcnames

#define	setjmp(e)	(ZSVJMP((e),&u_jmpstat),u_jmpstat)
#define	longjmp(e,v)	(u_jmpstat=(v),ZDOJMP((e),&u_jmpstat))

#endif	/* NOLIBCNAMES */

/* The following is necessary to prevent to prevent the optimizer from
 * doing unwise things with setjmp on a Sun-4.
 */
/* #pragma unknown_control_flow(zsvjmp_) */

#endif
