# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gki.h>

# GKIDECODE -- Decode the contents of one or more metacode files, printing
# the decoded metacode instructions in readable form on the standard output.

procedure t_gkidecode()

int	fd, list, verbose, gkiunits
pointer	gki, sp, fname
int	dd[LEN_GKIDD]

bool	clgetb()
int	clpopni(), clgfil(), clplen(), open(), btoi()
int	gki_fetch_next_instruction()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Open list of metafiles to be decoded.
	list = clpopni ("input")

	if (clgetb ("generic")) {
	    verbose = NO
	    gkiunits = NO
	} else {
	    verbose = btoi (clgetb ("verbose"))
	    gkiunits = btoi (clgetb ("gkiunits"))
	}

	# Set up the decoding graphics kernel.
	call gkp_install (dd, STDOUT, verbose, gkiunits)

	# Process a list of metacode files, writing the decoded metacode
	# instructions on the standard output.

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    # Print header if new file.
	    if (clplen (list) > 1) {
		call printf ("\n# METAFILE '%s':\n")
		    call pargstr (Memc[fname])
	    }

	    # Open input file.
	    iferr (fd = open (Memc[fname], READ_ONLY, BINARY_FILE)) {
		call erract (EA_WARN)
		next
	    } else
		call gkp_grstream (fd)

	    # Process the metacode.
	    while (gki_fetch_next_instruction (fd, gki) != EOF)
		call gki_execute (Mems[gki], dd)

	    call close (fd)
	}

	call clpcls (list)
	call sfree (sp)
end
