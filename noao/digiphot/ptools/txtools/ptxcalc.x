include <ctype.h>
include <error.h>
include <evexpr.h>
include "../../lib/ptkeysdef.h"

define	LEN_LONGLINE	10

# PT_XCALC -- Edit a field in a record using a value expression and a
# a selection expression.

int procedure pt_xcalc (tp_in, tp_out, field, value, expr)

int	tp_in		# the input text file descriptor
int	tp_out		# the output text file descriptor
char	field[ARB]	# field to be edited
char	value[ARB]	# the new value expression
char	expr[ARB]	# the expression to be evaluated

bool	oexpr
int	first_rec, nunique, uunique, funique, fieldno, fieldtype, fieldlen
int	elem, record, editall, ncontinue, recptr, nchars, buflen, lenrecord
int	offset, fieldptr
pointer	key, lline, o, v, sp, line, name, newvalue, fmtstr

bool	streq()
extern	pt_getop()
int	getline(), strncmp(), pt_kstati(), pt_fmkrec()
pointer	evexpr(), locpr()
errchk	evexpr(), locpr(), pt_getop()

begin
	# Get some working space.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (newvalue, SZ_FNAME, TY_CHAR)
	call salloc (fmtstr, SZ_FNAME, TY_CHAR)

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
	call malloc (lline, buflen, TY_CHAR)

	# Initilize the record read.
	ncontinue = 0
	recptr = 1
	fieldptr = 0

	# Initialize the expression decoding.
	o = NULL
	if (streq (expr, "yes") || streq (expr, "YES"))
	    editall = YES
	else
	    editall = NO
	v = NULL

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

		# Set the record size and set the column to be altered.
		if (recptr == 1) {
		    lenrecord = nchars
		    if (first_rec == YES) {
			call pt_kid (field, Memc[name], elem)
			fieldno = pt_kstati (key, Memc[name], KY_INDEX)
		        if (fieldno <= 0) {
			    call eprintf (
				"Cannot find field %s in input file\n")
			        call pargstr (Memc[name])
		            break
		        }
		        fieldtype = pt_kstati (key, Memc[name], KY_DATATYPE)
			if (fieldtype != TY_INT && fieldtype != TY_REAL) {
			    call eprintf ("Field %s is not numeric\n")
			        call pargstr (Memc[name])
		            break
			}
		        fieldlen = pt_kstati (key, Memc[name], KY_LENGTH)
		        call pt_kstats (key, Memc[name], KY_FMTSTR,
			    Memc[fmtstr], SZ_FNAME)
		    }
		} else
		    lenrecord = lenrecord + nchars

		# Build the record.
		offset = pt_fmkrec (key, fieldno, elem, Memc[line],
		    nchars, first_rec, recptr, ncontinue)
		if (offset > 0)
		    fieldptr = lenrecord - nchars + offset

		# Reallocate the temporary record space if necessary.
		if (lenrecord > buflen) {
		    buflen = buflen + SZ_LINE
		    call realloc (lline, buflen, TY_CHAR)
		}

		# Store the record.
		call amovc (Memc[line], Memc[lline+lenrecord-nchars], nchars)
		Memc[lline+lenrecord] = EOS

	        # Do the record bookkeeping.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Evaluate the value and selection expression.
		    iferr {

			if (editall == NO) {
			    call pt_apset (key)
			    o = evexpr (expr, locpr (pt_getop), 0)
		            if (O_TYPE(o) != TY_BOOL)
				call error (0,
				    "Selection expression must be a boolean")
			    oexpr = O_VALB(o)
			} else
			    oexpr = true

		        if (oexpr) {
			    call pt_apset (key)
			    v = evexpr (value, locpr (pt_getop), 0)
			    switch (fieldtype) {
			    case TY_BOOL:
				if (O_TYPE(v) != TY_BOOL) {
				    call error (0,
				        "Value must be a boolean expression")
				} else if (fieldptr > 0) {
				    call sprintf (Memc[newvalue], fieldlen,
					Memc[fmtstr])
					call pargb (O_VALB(v))
				    call amovc (Memc[newvalue],
				        Memc[lline+fieldptr-1], fieldlen)
				}
			    case TY_INT:
				if (O_TYPE(v) != TY_INT) {
				    call error (0,
				        "Value must be an integer expression")
				} else if (fieldptr > 0) {
				    call sprintf (Memc[newvalue], fieldlen,
					Memc[fmtstr])
					call pargi (O_VALI(v))
				    call amovc (Memc[newvalue],
				        Memc[lline+fieldptr-1], fieldlen)
				}
			    case TY_REAL:
				if (O_TYPE(v) != TY_REAL) {
				    call error (0,
				        "Value must be a real expression")
				} else if (fieldptr > 0) {
				    call sprintf (Memc[newvalue], fieldlen,
					Memc[fmtstr])
					call pargr (O_VALR(v))
				    call amovc (Memc[newvalue],
				        Memc[lline+fieldptr-1], fieldlen)
				}
			    case TY_CHAR:
				if (O_TYPE(v) != TY_CHAR) {
				    call error (0,
				        "Value must be a string expression")
				} else if (fieldptr > 0) {
				    call sprintf (Memc[newvalue], fieldlen,
					Memc[fmtstr])
					call pargstr (O_VALC(v))
				    call amovc (Memc[newvalue],
				        Memc[lline+fieldptr-1], fieldlen)
				}
			    default:
				call error (0,
				    "Value expression is undefined")
			    }
		        }

		    } then {
			call erract (EA_WARN)
			call error (0,
			    "Error evaluating the value expression")
		    }

		    # Write out the record.
		    call putline (tp_out, Memc[lline])

		    # Increment the record counter.
		    record = record + 1
		    first_rec = NO

		    # Reinitialize the record read.
		    ncontinue = 0
		    recptr = 1
		    fieldptr = 0
		    if (o != NULL) {
			call xev_freeop (o)
		        call mfree (o, TY_STRUCT)
		    }
		    o = NULL
		    if (v != NULL) {
			call xev_freeop (v)
		        call mfree (v, TY_STRUCT)
		    }
		    v = NULL
	        }
	    }

	}

	# Cleanup.
	call mfree (lline, TY_CHAR)
	call sfree (sp)
	call pt_kyfree (key)

	return (record)
end
