# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gki.h>

# NSPPKERN -- Graphics kernel for the NCAR System Plot Package graphics
# interface.

procedure t_nsppkern()

int	fd, list
pointer	gki, sp, fname, devname
int	dev[LEN_GKIDD], deb[LEN_GKIDD]
int	debug, verbose, gkiunits
bool	clgetb()
int	clpopni(), clgfil(), open(), btoi()
int	gki_fetch_next_instruction()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)

	# Open list of metafiles to be decoded.
	list = clpopni ("input")

	# Get parameters.
	call clgstr ("device", Memc[devname], SZ_FNAME)
	if (clgetb ("generic")) {
	    debug = NO
	    verbose = NO
	    gkiunits = NO
	} else {
	    debug   = btoi (clgetb ("debug"))
	    verbose = btoi (clgetb ("verbose"))
	    gkiunits = btoi (clgetb ("gkiunits"))
	}

	# Open the graphics kernel.
	call gkt_open (Memc[devname], dev)
	call gkp_install (deb, STDERR, verbose, gkiunits)

	# Process a list of metacode files, writing the decoded metacode
	# instructions on the standard output.

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    # Open input file.
	    iferr (fd = open (Memc[fname], READ_ONLY, BINARY_FILE)) {
		call erract (EA_WARN)
		next
	    }

	    # Process the metacode instruction stream.
	    while (gki_fetch_next_instruction (fd, gki) != EOF) {
		if (debug == YES)
		    call gki_execute (Mems[gki], deb)
		call gki_execute (Mems[gki], dev)
	    }

	    call close (fd)
	}

	call gkp_close()
	call gkt_close()
	call clpcls (list)
	call sfree (sp)
end
