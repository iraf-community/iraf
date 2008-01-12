include	<fset.h>

# T_TXRENUMBER -- Procedure to renumber standard APPHOT and DAOPHOT text
# output files. The ST TTOOLS task can be used for binary format.
# The program assumes that there is a column labelled ID.

procedure t_txrenumber ()

pointer	tp_in		# pointer to the input table
pointer	tp_out		# pointer to the output table
pointer	id		# name of the id column

int	inlist, nrecs, idoffset
pointer	sp, infile, outfile
int	clpopnu(), clgeti(), clgfil(), access(), open(), fstati(), pt_renumber()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (id, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	inlist = clpopnu ("textfiles")
	idoffset = clgeti ("idoffset")
	call clgstr ("id", Memc[id], SZ_FNAME)
	call strlwr (Memc[id])

	# Loop over the input files.
	while (clgfil (inlist, Memc[infile], SZ_FNAME) != EOF) {

	    # Open the input file.
	    if (access (Memc[infile], 0, TEXT_FILE) == YES)
	        tp_in = open (Memc[infile], READ_ONLY, TEXT_FILE)
	    else
		next

	    # Open an output text file.
	    call mktemp ("temp", Memc[outfile], SZ_FNAME)
	    tp_out = open (Memc[outfile], NEW_FILE, TEXT_FILE)

	    # Renumber the stars.
	    nrecs = pt_renumber (tp_in, tp_out, idoffset, Memc[id])

	    # Close up the input and output files.
	    call close (tp_in)
	    call close (tp_out)

	    # Rename the files.
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
