include	"../lib/apsel.h"

define	NRESULT		5

# DP_TPKINIT -- Procedure to initialize for reading the "standard" fields from
# a photometry table. The "standard" fields being ID, X, Y, MAG, ERR, and SKY

procedure dp_tpkinit (tp, colpoint)

pointer	tp			# the table descriptor
pointer	colpoint[ARB]		# the column descriptor

begin
	# Get the results one by one
	# First the ID
	call tbcfnd (tp, ID, colpoint[1], 1)
	if (colpoint[1] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (ID)
	}

	# Then the position
	call tbcfnd (tp, XCENTER, colpoint[2], 1)
	if (colpoint[2] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (XCENTER)
	}

	call tbcfnd (tp, YCENTER, colpoint[3], 1)
	if (colpoint[3] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (YCENTER)
	}

	# Now the Magnitude
	call tbcfnd (tp, MAG, colpoint[4], 1)
	if (colpoint[4] == NULL)		# No column
	    call tbcfnd (tp, APMAG, colpoint[4], 1)
	if (colpoint[4] == NULL) {
	    call eprintf ("Column %s not found\n")
	    call pargstr (APMAG)
	}

	# The sky
	call tbcfnd (tp, SKY, colpoint[5], 1)
	if (colpoint[5] == NULL)
	    call tbcfnd (tp, APSKY, colpoint[5], 1)
	if (colpoint[5] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (APSKY)
	}
end
