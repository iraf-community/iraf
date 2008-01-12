include	<fset.h>

# T_TXSORT -- Procedure to sort standard APPHOT and DAOPHOT text
# output files. The ST TTOOLS task can be used for binary format.

procedure t_txsort ()

int	tp_in		# input file descriptor
int	tp_out		# input file descriptor

int	inlist, ascend, nrecs
pointer	sp, infile, outfile, column
bool	clgetb()
int	clpopnu(), clgfil(), access(), btoi(), open(), fstati(), pt_sortnum()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (column, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	inlist = clpopnu ("textfiles")
	call clgstr ("field", Memc[column], SZ_FNAME)
	ascend = btoi (clgetb ("ascend"))

	# Check the column on which to sort.
	call strlwr (Memc[column])
	if (Memc[column] == EOS)
	    call error (0, "The sort column is undefined.")

	while (clgfil (inlist, Memc[infile], SZ_FNAME) != EOF) {

	    # Open the input file.
	    if (access (Memc[infile], 0, TEXT_FILE) == YES)
	        tp_in = open (Memc[infile], READ_ONLY, TEXT_FILE)
	    else
	        call error (0, "The input file is not a text file.")

	    # Open an output text file.
	    call mktemp ("temp", Memc[outfile], SZ_FNAME)
	    tp_out = open (Memc[outfile], NEW_FILE, TEXT_FILE)

	    # Sort the stars.
	    nrecs = pt_sortnum (tp_in, tp_out, Memc[column], ascend)

	    # Close up the input and output files.
	    call close (tp_in)
	    call close (tp_out)
	    if (nrecs <= 0)
	        call delete (Memc[outfile])
	    else {
	        call delete (Memc[infile])
	        call rename (Memc[outfile], Memc[infile])
	    }
	}

	call clpcls (inlist)
	call sfree (sp)
end	
