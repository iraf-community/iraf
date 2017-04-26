# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "zdisplay.h"
include "iis.h"

# IISBLK -- Blink IIS display frames at millisecond time resolution.

procedure iisblk (chan1, chan2, chan3, chan4, nframes, rate)

int	chan1[ARB]
int	chan2[ARB]
int	chan3[ARB]
int	chan4[ARB]
int	nframes
real	rate

int	msec, status, xcur, ycur
int	and()

begin
	status = 0
	msec = int (rate * 1000.)

	while (and (status, PUSH) == 0) {
	    call zwmsec (msec)
	    call iisrgb (chan1, chan1, chan1)
	    call zwmsec (msec)
	    call iisrgb (chan2, chan2, chan2)
	    if (nframes >= 3) {
	        call zwmsec (msec)
	        call iisrgb (chan3, chan3, chan3)
	    }
	    if (nframes == 4) {
	        call zwmsec (msec)
	        call iisrgb (chan4, chan4, chan4)
	    }
	    call iisrcr (status, xcur, ycur)
	}
end
