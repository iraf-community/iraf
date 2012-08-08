include	<ctype.h>
include	"reloperr.h"

define	TFIELDS		7
define	REQFIELD	3

# MJD -- Compute the modified julian date of a time expressed as a string
#
# Dates are of the form YYYYMMDD:HHMMSSCC (fields after the colon are optional).
# If an optional field is not present, its value is considered to be zero.
# Dates must be between 1 Jan 1858 and 31 Dec 2099
#
# B.Simon	7-Oct-87	First Code
# Phil Hodge	20-Feb-91	Move the data statements.

double procedure mjd (date)

char	date[ARB]	# i: String in the form YYYYMMDD:HHMMSSCC
#--
int	jd, datelen, it, ic
int	time[TFIELDS], tpos[2,TFIELDS], tlim[2,TFIELDS]
pointer	sp, errtxt
double	df

int	strlen()

string	badfmt	"Date has incorrect format (%s)"

data	tpos / 1, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17 /
data	tlim / 1858, 2099, 1, 12, 1, 31, 0, 23, 0, 59, 0, 59, 0, 99 /

begin
	# Allocate dynamic memory for error string

	call smark (sp)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	datelen = strlen (date)
	call aclri (time, TFIELDS)

	# Convert the date string into integer fields

	do it = 1, TFIELDS {

	    # Check for absence of optional fields

	    if (tpos[1,it] > datelen) {
		if (it > REQFIELD)
		    break
		else {
		    call sprintf (Memc[errtxt], SZ_LINE, badfmt)
		    call pargstr (date)
		    call error (SYNTAX, Memc[errtxt])
		}
	    }

	    # Convert a field in the date string to an integer

	    do ic = tpos[1,it], tpos[2,it] {
		if (IS_DIGIT(date[ic]))
		    time[it] = 10 * time[it] + TO_INTEG(date[ic])
		else {
		    call sprintf (Memc[errtxt], SZ_LINE, badfmt)
		    call pargstr (date)
		    call error (SYNTAX, Memc[errtxt])
		}
	    }

	    # Do bounds checking on the field
	    # Some errors can slip thru, e.g., Feb 30

	    if ((time[it] < tlim[1,it]) || (time[it] > tlim[2,it])) {
		call sprintf (Memc[errtxt], SZ_LINE, badfmt)
		call pargstr (date)
		call error (SYNTAX, Memc[errtxt])
	    }
	}

	# Compute integer part of modified julian date
	# From Van Flandern & Pulkkinen ApJ Sup 41:391-411 Nov 79

	jd = 367 * time[1] - 7 * (time[1] + (time[2] + 9) / 12) / 4 -
	     3 * ((time[1] + (time[2] - 9) / 7) / 100 + 1) / 4 +
	     275 * time[2] / 9 + time[3] - 678971

	# Compute fractional part of modified julian date
	# N.B. julian date begins at noon, modified julian date at midnight

	df = double (time[7] + 100 * (time[6] + 60 * 
	             (time[5] + 60 * time[4]))) / 8640000.0

	call sfree (sp)
	return (jd + df)
end
