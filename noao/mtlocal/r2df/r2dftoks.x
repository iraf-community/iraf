include	<mach.h>
include <imhdr.h>
include "r2df.h"

define	LEN_KEYWORD	8


# R2DFSTORE_TOKEN -- Store 2D-FRUTTI specific keywords in the IRAF
# image header.

procedure r2dfstore_token (parameters, im)

short	parameters[ARB]		# Pointer to program data structure
pointer	im			# Pointer to image

int	fd
int	stropen()
errchk	stropen, r2dfsicard

begin
	# Open image user area as a string
	fd = stropen (UNKNOWN(im), (LEN_USER_AREA - 1) * SZ_STRUCT, WRITE_ONLY)

	# FITS keyword are formatted and appended to the image user area.
	call r2dfsicard (fd, "ITIME", ITIME(parameters),
	    "REQUESTED INTEGRATION TIME (SECS)") 
	call r2dfsicard (fd, "OTIME", OTIME(parameters),
	    "ACTUAL INTEGRATION TIME (SECS)")

	call close (fd)
end


# R2DFFCARD -- Format and append a FITS header card with a real keyword value
# to the input string buffer.  

procedure r2dffcard (fd, keyword, value, comment, precision)

int	fd			# File descriptor of input string buffer
char	keyword[ARB]		# FITS keyword
real	value			# Value of FITS keyword
char	comment[ARB]		# Comment string
int	precision		# Number of decimal places output

begin
	call fprintf (fd, "%-8.8s= %20.*f  /  %-45.45s\n")
	    call pargstr (keyword)
	    call pargi (precision)
	    call pargr (value)
	    call pargstr (comment)
end


# R2DFSICARD -- Format and append a FITS header card with a short integer
# keyword value to the input string buffer.

procedure r2dfsicard (fd, keyword, value, comment)

int	fd			# File descriptor of input string buffer
char	keyword[ARB]		# FITS keyword
short	value			# Value of FITS keyword
char	comment[ARB]		# Comment string

begin
	call fprintf (fd, "%-8.8s= %20d  /  %-45.45s\n")
	    call pargstr (keyword)
	    call pargs (value)
	    call pargstr (comment)
end


# R2DFHMSCARD -- Format and append a FITS header card to the input string
# buffer.  The value is input as 3 short integers; it is output in HH:MM:SS
# format with %h.  The procedure can be used for RA, DEC and ST, UT and HA.

procedure r2dfhmscard (fd, keyword, hours, minutes, seconds, comment)

int	fd			# File descriptor
char	keyword[ARB]		# FITS keyword
short	hours			# Hours
short	minutes			# Minutes
short	seconds			# Seconds
char	comment			# Comment string

begin
	call fprintf (fd, "%-8.8s= '%c%02d:%02d:%02d'  /  %-54.54s\n")
	    call pargstr (keyword)
	    if (hours < 0)
		call pargc ('-')
	    else
		call pargc (' ')
	    call pargs (hours)
	    call pargs (minutes)
	    call pargs (seconds)
	    call pargstr (comment)
end


# R2DFYMDCARD - Format and append a FITS header card to the input
# string buffer.  The value is input as 3 short integers; it is output
# in the format dd-mm-yy the format dd-mm-yy.

procedure r2dfymdcard (fd, keyword, years, months, days, comment)

int	fd			# File descriptor
char	keyword[ARB]		# FITS keyword
short	years			# Hours
short	months			# Minutes
short	days			# Seconds
char	comment			# Comment string

begin
	call fprintf (fd, "%-8.8s= '%02d-%02d-%4d'  /  %-55.55s\n")
	    call pargstr (keyword)
	    call pargs (days)
	    call pargs (months)
	    call pargs (years)
	    call pargstr (comment)
end


# R2DFOBSCARD -- Procedure to code the object type into a FITS card.

procedure r2dfobscard (fd, keyword, data_code, comment)

int	fd			# File descriptor
char	keyword[ARB]		# FITS keyword
short	data_code		# type of data
char	comment[ARB]		# coment string

char	str[LEN_OBJECT+1]
int	maxch, nblanks
int	strlen()

begin
	switch (data_code) {
	case OBJECT:
	    call strcpy ("OBJECT", str, LEN_OBJECT)
	case DARK:
	    call strcpy ("DARK", str, LEN_OBJECT)
	case PFLAT:
	    call strcpy ("PROJECTOR FLAT", str, LEN_OBJECT)
	case SFLAT:
	    call strcpy ("SKY FLAT", str, LEN_OBJECT)
	case COMP:
	    call strcpy ("COMPARISON LAMP", str, LEN_OBJECT)
	case BIAS:
	    call strcpy ("BIAS", str, LEN_OBJECT)
	case DFLAT:
	    call strcpy ("DOME FLAT", str, LEN_OBJECT)
	case MASK:
	    call strcpy ("MASK", str, LEN_OBJECT)
	case MULT:
	    call strcpy ("MULT", str, LEN_OBJECT)
	case SCAN:
	    call strcpy ("SCAN", str, LEN_OBJECT)
	}
	call sprintf (str[strlen (str) + 1], LEN_OBJECT, " (%d)")
	    call pargs (data_code)

	maxch = strlen (str)
	maxch = max (maxch, LEN_KEYWORD)
	nblanks = LEN_OBJECT - maxch

	call fprintf (fd, "%-8.8s= '%*.*s'  /  %*.*s\n")
	    call pargstr (keyword)
	    call pargi (-maxch)
	    call pargi (maxch)
	    call pargstr (str)
	    call pargi (-nblanks)
	    call pargi (nblanks)
	    call pargstr (comment)
end
