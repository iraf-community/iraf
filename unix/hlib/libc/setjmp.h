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

typedef	XINT	jmp_buf[LEN_JUMPBUF];
static	XINT	u_jmpstat;

extern  void  longjmp (jmp_buf env, int val);
extern  void  ZSVJMP (XINT *jmpbuf, XINT *status);
extern  void  ZDOJMP (XINT *jmpbuf, XINT *status);

#define	setjmp(e)	(ZSVJMP((e),&u_jmpstat),u_jmpstat)
#define	longjmp(e,v)	(u_jmpstat=(v),ZDOJMP((e),&u_jmpstat))

#define	D_setjmp
