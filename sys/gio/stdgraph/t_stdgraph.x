# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gset.h>
include	<gki.h>

define	SZ_TXQUALITY	1

# STDGRAPH -- Graphics kernel for the standard graphics output (interactive
# graphics terminal).

procedure t_stdgraph()

int	fd, list
char	txquality[SZ_TXQUALITY]
pointer	gki, sp, fname, devname
int	dev[LEN_GKIDD], deb[LEN_GKIDD]
int	debug, verbose, gkiunits, xres, yres, quality
bool	clgetb()
int	clpopni(), clgfil(), open(), btoi(), clgeti()
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
	    quality = 0
	    xres = 0
	    yres = 0

	} else {
	    debug = btoi (clgetb ("debug"))
	    if (debug == YES) {
		verbose = btoi (clgetb ("verbose"))
		gkiunits = btoi (clgetb ("gkiunits"))
	    }

	    # Get the quality parameter for the text generator.
	    call clgstr ("txquality", txquality, SZ_TXQUALITY)
	    switch (txquality[1]) {
	    case 'l':
		quality = GT_LOW
	    case 'm':
		quality = GT_MEDIUM
	    case 'h':
		quality = GT_HIGH
	    default:
		quality = 0
	    }

	    xres     = clgeti ("xres")
	    yres     = clgeti ("yres")
	}

	# Open the graphics kernel.

	call stg_open (Memc[devname], dev, STDIN, STDOUT, xres, yres, quality)
	call gkp_install (deb, STDERR, verbose, gkiunits)

	# Process a list of metacode files, writing the decoded metacode
	# instructions on the standard output.

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    # Open input file.
	    iferr (fd = open (Memc[fname], READ_ONLY, BINARY_FILE)) {
		call erract (EA_WARN)
		next
	    } else
		call stg_grstream (fd)

	    # Process the metacode instruction stream.
	    while (gki_fetch_next_instruction (fd, gki) != EOF)
		switch (Mems[gki+GKI_HDR_OPCODE-1]) {
		case GKI_CLOSEWS, GKI_DEACTIVATEWS, GKI_REACTIVATEWS:
		    # These instructions are passed directly to the kernel via
		    # the PSIOCTRL stream at runtime, but are ignored in
		    # metacode to avoid unnecessary mode switching of the
		    # terminal.
		    ;
		default:
		    if (debug == YES)
			call gki_execute (Mems[gki], deb)
		    call gki_execute (Mems[gki], dev)
		}

	    call close (fd)
	}

	# Make sure we finish with CLOSEWS so that the terminal is left in
	# text mode.

	call stg_closews (NULL, NULL)

	# Finish up.
	call gkp_close()
	call stg_close()
	call clpcls (list)
	call sfree (sp)
end
