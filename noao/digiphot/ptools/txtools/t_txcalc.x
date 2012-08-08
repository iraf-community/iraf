include	<fset.h>

# T_TXCALC -- Edit a field in an APPHOT/DAOPHOT text data base using a
# value expression.

procedure t_txcalc ()

pointer	infile		# the input file list
pointer	outfile		# the output file list
pointer	field		# pointer to the field to be edited
pointer	value		# pointer to the value expression string

int	inlist, tp_in, tp_out, nrecs
pointer	sp
int	clpopnu(), access(), open(), fstati(), clgfil(), pt_xcalc()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (field, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_LINE, TY_CHAR)

	# Get the various task parameters.
	inlist = clpopnu ("textfiles")
	#outlist = clpopnu ("outfiles")
	call clgstr ("field", Memc[field], SZ_FNAME)
	call strupr (Memc[field])
	call clgstr ("value", Memc[value], SZ_LINE)

	while (clgfil (inlist, Memc[infile], SZ_FNAME) != EOF) {

	    # Open the input file.
	    if (access (Memc[infile], 0, TEXT_FILE) == YES)
	        tp_in = open (Memc[infile], READ_ONLY, TEXT_FILE)
	    else
	        call error (0, "The input file is a binary file.")

	    # Open an output text file.
	    call mktemp ("temp", Memc[outfile], SZ_FNAME)
	    tp_out = open (Memc[outfile], NEW_FILE, TEXT_FILE)

	    # Select the stars.
	    nrecs = pt_xcalc (tp_in, tp_out, Memc[field], Memc[value], "yes")

	    # Close up the input and output files.
	    call close (tp_in)
	    call close (tp_out)

	    if (nrecs <= 0) {
		call delete (Memc[outfile])
	    } else {
		call delete (Memc[infile])
		call rename (Memc[outfile], Memc[infile])
	    }
	}

	call clpcls (inlist)
	call sfree (sp)
end	
