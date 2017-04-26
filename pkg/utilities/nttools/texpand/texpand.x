include	<fset.h>

# TEXPAND -- Expand the rows of a table according to a set of rules
#
# B.Simon	25-Apr-88	Original
# Phil Hodge	 4-Oct-95	Use table name template routines tbnopenp, etc.

procedure texpand ()

#--
pointer	ilist		# Input file name template
pointer	olist		# Output file name template
pointer	rbase		# Name of file containing expansion rules
pointer debug		# Debug file name
bool	verbose		# Diagnostic message flag

int	junk, dbg
pointer	sp, itp, otp, input, output, target, action

bool	clgetb()
int	open(), tbnlen(), tbnget()
pointer	tbnopenp(), tbtopn()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (rbase, SZ_FNAME, TY_CHAR)
	call salloc (debug, SZ_FNAME, TY_CHAR)

	# Read the parameter file

	ilist = tbnopenp ("input")
	olist = tbnopenp ("output")
	call clgstr ("rbase", Memc[rbase], SZ_FNAME)
	call clgstr ("debug", Memc[debug], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Open debug file

	if (Memc[debug] == ' ' || Memc[debug] == EOS)
	    dbg = NULL
	else
	    dbg = open (Memc[debug], NEW_FILE, TEXT_FILE)

	# Check to see that input & output templates
	# have same number of files

	if (tbnlen (ilist) != tbnlen (olist))
	    call error (ERR, "Number of input and output tables do not match")

	while (tbnget (ilist, Memc[input], SZ_FNAME) != EOF) {

	    junk = tbnget (olist, Memc[output], SZ_FNAME)

	    # Open input and output tables

	    itp = tbtopn (Memc[input], READ_ONLY, NULL)
	    otp = tbtopn (Memc[output], NEW_COPY, itp)
	    call tbtcre (otp)
	    call tbhcal (itp, otp)

	    # Create target and action tables from the rule base

	    call parser (Memc[rbase], itp, dbg, target, action)

	    # Expand the rows of the input table using the rules
	    # encoded in the target and action tables

	    call use_rules (itp, otp, target, action, dbg, verbose)

	    # Print diagnostic message and close tables 

	    if (verbose) {
		call tbtnam (itp, Memc[input], SZ_FNAME)
		call tbtnam (otp, Memc[output], SZ_FNAME)

		call printf ("%s -> %s\n")
		    call pargstr (Memc[input])
		    call pargstr (Memc[output])
		call flush (STDOUT)
	    }

	    call tbtclo (itp)
	    call tbtclo (otp)
	}

	call close (dbg)
	call tbnclose (ilist)
	call tbnclose (olist)
	call sfree (sp)	
end
