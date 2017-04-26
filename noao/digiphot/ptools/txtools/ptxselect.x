include <ctype.h>
include <error.h>
include <evexpr.h>
include "../../lib/ptkeysdef.h"

define	LEN_LONGLINE	10

# PT_XSELECT -- Select records based on evaluating a logical expression.

int procedure pt_xselect (tp_in, tp_out, expr)

int	tp_in		# the input text file descriptor
int	tp_out		# the output text file descriptor
char	expr[ARB]	# the expression to be evaluated

bool	oexpr
int	first_rec, nunique, uunique, funique, record, printall
int	ncontinue, recptr, nchars, buflen, lenrecord
pointer	key, line, lline, o

bool	streq()
extern	pt_getop()
int	getline(), strncmp()
pointer	evexpr(), locpr()
errchk	evexpr(), pt_getop()

begin
	# Initialize keyword structure.
	call pt_kyinit (key)

	# Initialize the file read.
	first_rec = YES
	nunique = 0
	uunique = 0
	funique = 0
	record = 0

	# Initialize the buffers.
	buflen = LEN_LONGLINE * SZ_LINE
	call malloc (line, SZ_LINE, TY_CHAR)
	call malloc (lline, buflen, TY_CHAR)

	# Initilize the record read.
	ncontinue = 0
	recptr = 1

	# Initialize the expression decoding.
	o = NULL
	if (streq (expr, "yes")) {
	    oexpr = true
	    printall = YES
	} else {
	    oexpr = false
	    printall = NO
	}

	# Loop over the text file records.
	repeat  {

	    # Read in a line of the text file.
	    nchars = getline (tp_in, Memc[line])
	    if (nchars == EOF)
		break

	    # Determine the type of record.
	    if (Memc[line] == KY_CHAR_POUND) {

	        if (strncmp (Memc[line], KY_CHAR_KEYWORD, KY_LEN_STR) == 0) {
		    call pt_kyadd (key, Memc[line], nchars)
		    call putline (tp_out, Memc[line])
	        } else if (strncmp (Memc[line], KY_CHAR_NAME,
		    KY_LEN_STR) == 0) {
		    nunique = nunique + 1
		    call pt_kname (key, Memc[line], nchars, nunique)
		    call putline (tp_out, Memc[line])
	        } else if (strncmp (Memc[line], KY_CHAR_UNITS,
		    KY_LEN_STR) == 0) {
		    uunique = uunique + 1
		    call pt_knunits (key, Memc[line], nchars, uunique)
		    call putline (tp_out, Memc[line])
	        } else if (strncmp (Memc[line], KY_CHAR_FORMAT,
		    KY_LEN_STR) == 0) {
		    funique = funique + 1
		    call pt_knformats (key, Memc[line], nchars, funique)
		    call putline (tp_out, Memc[line])
	        } else {
		    # skip lines beginning with # sign
		    call putline (tp_out, Memc[line])
	        }

	    } else if (Memc[line] == KY_CHAR_NEWLINE) {
		# skip blank lines
		call putline (tp_out, Memc[line])

	    } else {

		# Reset the record size.
		if (recptr == 1)
		    lenrecord = nchars
		else
		    lenrecord = lenrecord + nchars

		# Build the record.
		call pt_mkrec (key, Memc[line], nchars, first_rec, recptr,
		    ncontinue) 

		# Reallocate the temporary record space.
		if (lenrecord > buflen) {
		    buflen = buflen + SZ_LINE
		    call realloc (lline, buflen, TY_CHAR)
		}

		# Store the record.
		call amovc (Memc[line], Memc[lline+lenrecord-nchars], nchars)
		Memc[lline+lenrecord] = EOS

	        # Do the record bookkeeping.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Evaluate the expression.
		    iferr {
			if (printall == NO) {
			    call pt_apset (key)
			    o = evexpr (expr, locpr (pt_getop), 0)
		            if (O_TYPE(o) != TY_BOOL)
				call error (0, "Expression must be a boolean")
			    oexpr = O_VALB(o)
			}
		    } then {
			call erract (EA_WARN)
			call error (0, "Error evaluating selection expression")
		    }

		    # Write out the expression.
		    if (oexpr)
			call putline (tp_out, Memc[lline])

		    # Increment the record counter.
		    record = record + 1
		    first_rec = NO

		    # Reinitialize the record read.
		    ncontinue = 0
		    recptr = 1
		    if (o != NULL) {
			call xev_freeop (o)
		        call mfree (o, TY_STRUCT)
		    }
	        }
	    }

	}

	# Cleanup.
	call mfree (line, TY_CHAR)
	call mfree (lline, TY_CHAR)
	call pt_kyfree (key)

	return (record)
end
