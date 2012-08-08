# T_TXDUMP -- Procedure to perform a relational select operation upon a set of
# records within a text file. Our function is to select all records from the
# input file matching some criterion, printing the listed fields on the
# standard output. Dumping the keywords and reheadersting is optional.

procedure t_txdump ()

pointer	textfile		# list of input text files
pointer	fields			# list of fields to be dumped
pointer	expr			# boolean expression to be evaluated
int	headers			# format the output
int	parameters		# print the headers

int	list, fd
pointer	sp
bool	clgetb()
int	clpopnu(), clgfil(), open(), btoi()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (textfile, SZ_FNAME, TY_CHAR)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)

	# Get the parameters.
	list = clpopnu ("textfiles")
	call clgstr ("fields", Memc[fields], SZ_LINE)
	call strupr (Memc[fields])
	call clgstr ("expr", Memc[expr], SZ_LINE)
	headers = btoi (clgetb ("headers"))
	parameters = btoi (clgetb ("parameters"))

	# Select records.
	while (clgfil (list, Memc[textfile], SZ_FNAME) != EOF) {
	    fd = open (Memc[textfile], READ_ONLY, TEXT_FILE)
	    if (Memc[fields] != EOS)
	        call pt_xdump (fd, Memc[fields], Memc[expr], headers,
		    parameters)
	    call close (fd)
	}

	call clpcls (list)
	call sfree (sp)
end
