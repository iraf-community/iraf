include <mach.h>
include	"vt.h"

# DECODEHEADER -- Unpack date and time, and, if 'verbose' flag is set,
# display some information to the user.

procedure decodeheader (hbuf, hs, verbose)

pointer	hbuf		# header data input buffer pointer (short, SZ_VTHDR)
pointer	hs		# header data structure
bool	verbose		# verbose flag

int	hour, minute, second
int	bitupk()

begin
	# Unpack date, time.  The constants below are explained in the
	# description of the image header and how it is packed.  If any
	# changes are made the following code will have to be rewritten.

	# Month. The month and day are stored in the first header word.
	VT_HMONTH[hs] = (bitupk (int(Mems[hbuf]), 13, 4)) * 10 +
	    bitupk (int(Mems[hbuf]), 9, 4)

	# Day.
	VT_HDAY[hs] = (bitupk (int(Mems[hbuf]), 5, 4)) * 10 +
	    bitupk (int(Mems[hbuf]), 1, 4)

	# Year. The year is stored in the second header word.
	VT_HYEAR[hs] = (bitupk (int(Mems[hbuf+1]), 13, 4)) * 10 +
	    bitupk (int(Mems[hbuf+1]), 9, 4)

	# Time (seconds since midnight).  Stored in the third and forth words.
	VT_HTIME[hs] = (bitupk (int(Mems[hbuf+2]), 1, 2)) * 2**15 +
	    bitupk (int(Mems[hbuf+3]), 1, 15)

	# Store other header parameters. Stored one per word.
	VT_HWVLNGTH[hs]  = Mems[hbuf+4]	# Wavelength (angstroms)
	VT_HOBSTYPE[hs]  = Mems[hbuf+5]	# Observation type (0,1,2,3,or 4)
	VT_HAVINTENS[hs] = Mems[hbuf+6]	# Average intensity
	VT_HNUMCOLS[hs]  = Mems[hbuf+7]	# Number of columns
	VT_HINTGPIX[hs]  = Mems[hbuf+8]	# Integrations per pixel
	VT_HREPTIME[hs]  = Mems[hbuf+9]	# Repitition time

	# Calculate the time in hours, minutes, and seconds instead of
	# seconds since midnight.

	hour = int(VT_HTIME[hs]/3600)
	minute = int((VT_HTIME[hs] - hour * 3600)/60)
	second = VT_HTIME[hs] - hour * 3600 - minute * 60

	# If verbose, print out some header info on one line no <CR>.
	if (verbose) {
	    call printf ("%02d/%02d/%02d %02d:%02d:%02d")
		call pargi (VT_HMONTH[hs])
		call pargi (VT_HDAY[hs])
		call pargi (VT_HYEAR[hs])
		call pargi (hour)
		call pargi (minute)
		call pargi (second)
	    call printf (" wvlngth %d obstype %d numcols %d")
		call pargi (VT_HWVLNGTH[hs])
		call pargi (VT_HOBSTYPE[hs])
		call pargi (VT_HNUMCOLS[hs])
	    call flush (STDOUT)
	}
end
