/* SETJMP, LONGJMP -- Non local goto.  Requires libc.h and knames.h.
 * Note that jmp_buf must be at least one int larger than necessary to
 */
typedef	int	jmp_buf[LEN_JUMPBUF];
static	int	u_jmpstat;

#define	setjmp(e)	(ZSVJMP((e),&u_jmpstat),u_jmpstat)
#define	longjmp(e,v)	(u_jmpstat=(v),ZDOJMP((e),&u_jmpstat))

/* This causes an "undefined control" error on Sun-3s.
 * #ifdef sparc
 * extern	zsvjmp_();
 * #pragma unknown_control_flow(zsvjmp_)
 * #endif
 */

#define	D_setjmp
