# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gki.h>

# IMDKERN -- Graphics kernel for an image display frame buffer using the
# data stream interface.  The package is based on the SGI kernel.

procedure t_imdkern()

int	fd, list, dbfd
pointer	gki, sp, fname, devname, dbfname
int	dev[LEN_GKIDD], deb[LEN_GKIDD]
int	debug, verbose, gkiunits
int	color, frame

bool	clgetb()
int	clgeti(), envfind()
int	clpopni(), clgfil(), open(), btoi()
int	gki_fetch_next_instruction()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)
	call salloc (dbfname, SZ_PATHNAME, TY_CHAR)

	# Open list of metafiles to be decoded.
	list = clpopni ("input")

	# Set parameter defaults.
	debug = NO
	verbose = NO
	gkiunits = NO
	frame = -1
	color = -1

	# Check for global kernel debug flag.
	if (envfind ("idkdebug", Memc[dbfname], SZ_PATHNAME) > 0)
	    iferr (dbfd = open (Memc[dbfname], APPEND, TEXT_FILE)) {
		debug = NO
		dbfd = 0
	    } else
		debug = YES

	# Get parameters.
	call clgstr ("device", Memc[devname], SZ_FNAME)
	if (!clgetb ("generic")) {
	    debug   = btoi (clgetb ("debug"))
	    verbose = btoi (clgetb ("verbose"))
	    gkiunits = btoi (clgetb ("gkiunits"))
	    frame = clgeti ("frame")
	    color = clgeti ("color")
	}

	if (debug == YES && dbfd == 0)
	    dbfd = STDERR

	# Open the graphics kernel.
	call imd_opendev (Memc[devname], frame, color, dev)
	call gkp_install (deb, dbfd, verbose, gkiunits)

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
		if (debug == YES) {
		    call gki_execute (Mems[gki], deb)
		    call flush (dbfd)
		}
		call gki_execute (Mems[gki], dev)
	    }

	    call close (fd)
	}

	call gkp_close()
	call imd_close()
	call clpcls (list)
	call sfree (sp)
end
