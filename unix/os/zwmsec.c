/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#include <sys/time.h>
#include <signal.h>

#define	mask(s)	(1<<((s)-1))
#define	setvec(vec, a) \
	vec.sv_handler = a; vec.sv_mask = vec.sv_onstack = 0

static int ringring;


/* ZWMSEC -- Suspend task execution (sleep) for the specified number
 * of milliseconds.
 */
ZWMSEC (msec)
XINT	*msec;
{
	struct	itimerval itv, oitv;
	register struct itimerval *itp = &itv;
	struct	sigvec vec, ovec;
#ifdef SUNOS4
	void	napmsx();
#else
	int	napmsx();
#endif
	int	omask;

	if (*msec == 0)
	    return;

	timerclear (&itp->it_interval);
	timerclear (&itp->it_value);
	if (setitimer (ITIMER_REAL, itp, &oitv) < 0)
	    return;

	setvec (ovec, SIG_DFL);
	omask = sigblock(0);

	itp->it_value.tv_usec = (*msec % 1000) * 1000;
	itp->it_value.tv_sec  = (*msec / 1000);

	if (timerisset (&oitv.it_value)) {
	    if (timercmp(&oitv.it_value, &itp->it_value, >))
		oitv.it_value.tv_sec -= itp->it_value.tv_sec;
	    else {
		itp->it_value = oitv.it_value;
		/* This is a hack, but we must have time to
		 * return from the setitimer after the alarm
		 * or else it'll be restarted.  And, anyway,
		 * sleep never did anything more than this before.
		 */
		oitv.it_value.tv_sec  = 1;
		oitv.it_value.tv_usec = 0;
	    }
	}

	setvec (vec, napmsx);
	(void) sigvec (SIGALRM, &vec, &ovec);
	ringring = 0;
	(void) setitimer (ITIMER_REAL, itp, (struct itimerval *)0);

	while (!ringring)
	    sigpause (omask &~ mask(SIGALRM));

	(void) sigvec (SIGALRM, &ovec, (struct sigvec *)0);
	(void) setitimer (ITIMER_REAL, &oitv, (struct itimerval *)0);
}


#ifdef SUNOS4
static void
#else
static int
#endif
napmsx()
{
	ringring = 1;
}
