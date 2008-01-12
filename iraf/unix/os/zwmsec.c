/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

/* Comment out or ifdef the following if usleep is not available. */
#define USE_USLEEP

#ifdef USE_USLEEP
#define ONEHOUR (60 * 60 * 1000)


/* ZWMSEC -- Suspend task execution (sleep) for the specified number
 * of milliseconds.
 */
int ZWMSEC ( XINT *msec )
{
	/* Usleep doesn't really appear to be a standard, but it is
	 * available on most platforms.
	 */
	if (*msec > ONEHOUR)
	    sleep (*msec / 1000);
	else
	    (void) usleep ((unsigned long)(*msec) * 1000);

	return XOK;
}


#else
#include <sys/time.h>
#include <signal.h>

#define mask(s)	(1<<((s)-1))

static int ringring;
static void napmsx( int );


/* ZWMSEC -- Suspend task execution (sleep) for the specified number
 * of milliseconds.
 */
int ZWMSEC ( XINT *msec )
{
	struct itimerval itv, oitv;
	struct itimerval *itp = &itv;
	signal_handler_t sv_handler;
	int omask;

	if (*msec == 0)
	    return XOK;

	timerclear (&itp->it_interval);
	timerclear (&itp->it_value);
	if (setitimer (ITIMER_REAL, itp, &oitv) < 0)
	    return XOK;

#ifndef SOLARIS
	omask = sigblock(0);
#endif

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

	ringring = 0;
	sv_handler = signal (SIGALRM, &napmsx);
	(void) setitimer (ITIMER_REAL, itp, (struct itimerval *)0);

	while (!ringring)
#ifdef SOLARIS
	    sigpause (SIGALRM);
#else
	    sigpause (omask &~ mask(SIGALRM));
#endif

	signal (SIGALRM, sv_handler);
	(void) setitimer (ITIMER_REAL, &oitv, (struct itimerval *)0);

	return XOK;
}


static void napmsx( int junk )
{
	ringring = 1;
}
#endif
