include	"../lib/apseldef.h"

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
	    call tbcfnd (tp, SKY, colpoint[5], 1)
	if (colpoint[5] == NULL) {
	    call eprintf ("Column %s not found\n")
	        call pargstr (SKY)
	}
end


# DP_RRPHOT -- Fetch the photometry for a single star from either a
# table or a text photometry file.

int procedure dp_rrphot (tp, key, fields, indices, id, x, y, sky, mag,
	instar, nrow)

int	tp		# the input file descriptor
pointer	key		# pointer to text apphot structure
char	fields[ARB]	# character fields
int	indices[ARB]	# columns pointers
int	id		# star id
real	x		# x center value
real	y		# y center value
real	sky		# sky value
real	mag		# magnitude
int	instar		# current record
int	nrow		# maximum number of rows for ST table

bool	nullflag
int	nrec
int	dp_apsel()

begin
	# If nrow is 0 the file file is a text file otherwise it is a table.

	if (nrow == 0) {

	    nrec = dp_apsel (key, tp, fields, indices, id, x, y, sky, mag)
	    if (nrec != EOF)
		instar = instar + 1

	} else if ((instar + 1) <= nrow) {

	    instar = instar + 1
	    call tbrgti (tp, indices[1], id, nullflag, 1, instar)
	    call tbrgtr (tp, indices[2], x, nullflag, 1, instar)
	    call tbrgtr (tp, indices[3], y, nullflag, 1, instar)
	    call tbrgtr (tp, indices[4], mag, nullflag, 1, instar)
	    call tbrgtr (tp, indices[5], sky, nullflag, 1, instar)
	    nrec = instar

	} else
	    nrec = EOF

	return (nrec)
end
