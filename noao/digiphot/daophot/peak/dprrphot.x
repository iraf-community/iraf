# DP_RRPHOT -- Fetch the photometry for a single star from either a
# table or a text photometry file.

int procedure dp_rrphot (tp, key, fields, indices, id, x, y, sky, mag,
	instar, nrow)

pointer	tp		# pointer to the input table
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
