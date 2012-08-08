include	<fset.h>

# T_TXSELECT -- Select records from an APPHOT file based on the value of
# a logical expression.

procedure t_txselect ()

int	tp_in		# input file descriptor
int	tp_out		# output file descriptor
pointer	expr		# pointer to the expression string

int	inlist, outlist
pointer	sp, infile, outfile
int	clpopnu(), clplen(), access(), open(), fstati(), clgfil(), pt_xselect()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)

	# Get the various task parameters.
	inlist = clpopnu ("textfiles")
	outlist = clpopnu ("outfiles")
	call clgstr ("expr", Memc[expr], SZ_LINE)

	# Check that the input and output file lists have the
	# same length.
	if (clplen (inlist) != clplen (outlist))
	    call error (0,
	        "Input and output file lists are not the same length")

	while ((clgfil (inlist, Memc[infile], SZ_FNAME) != EOF) &&
	    (clgfil (outlist, Memc[outfile], SZ_FNAME) != EOF)) {

	    # Open the input file.
	    if (access (Memc[infile], 0, TEXT_FILE) == YES)
	        tp_in = open (Memc[infile], READ_ONLY, TEXT_FILE)
	    else
	        call error (0, "The input file is a binary file.")

	    # Open an output text file.
	    tp_out = open (Memc[outfile], NEW_FILE, TEXT_FILE)

	    # Select the stars.
	    if (pt_xselect (tp_in, tp_out, Memc[expr]) <= 0) {
		call eprintf ("File: %s is empty\n")
		    call pargstr (Memc[infile])
	    }

	    # Close up the input and output files.
	    call close (tp_in)
	    call close (tp_out)
	}

	call clpcls (inlist)
	call clpcls (outlist)
	call sfree (sp)
end	
