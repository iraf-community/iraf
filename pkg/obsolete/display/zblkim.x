# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"zdisplay.h"

# ZBLKIM -- Blink binary file display device (millisecond time resolution).

procedure zblkim (chan1, chan2, chan3, chan4, nframes, rate)

int	chan1[ARB]
int	chan2[ARB]
int	chan3[ARB]
int	chan4[ARB]
int	nframes
real	rate
int	device

begin
	device = chan1[1] / DEVCODE
	switch (device) {
	case IIS_CHAN:
	    call iisblk (chan1, chan2, chan3, chan4, nframes, rate)
	}
end
