# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gki.h>
include <gset.h>
include <mach.h>
include "ccp.h"

define	SZ_TXQUALITY	1

# CALCOMP -- Graphics kernel for Calcomp pen plotter output.  The whole
# package is copied as much as possible from the NSPP kernel.

procedure t_calcomp()

int	fd, list
pointer	gki, sp, fname, devname
int	dev[LEN_GKIDD], deb[LEN_GKIDD]
int	debug, verbose, gkiunits
char	txquality[SZ_TXQUALITY]
bool	clgetb()
char	clgetc()
real	clgetr()
int	clpopni(), clgfil(), open(), btoi()
int	gki_fetch_next_instruction()

include "ccp.com"

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)

	# Open list of metafiles to be decoded.
	list = clpopni ("input")

	# Get parameters.
	call clgstr ("device", Memc[devname], SZ_FNAME)

	if (clgetb ("generic")) {
	    debug 	  = NO
	    verbose 	  = NO
	    gkiunits 	  = NO
	    g_xtask_scale = INDEF
	    g_xndcto_p 	  = INDEF
	    g_ytask_scale = INDEF
	    g_yndcto_p 	  = INDEF
	    g_txquality   = 0
	    g_lwtype	  = 'n'
	    g_ltover      = false
	    g_lwover      = true
	    g_lcover      = false
	    g_dashlen     = INDEF
	    g_gaplen      = INDEF
	    g_plwsep      = INDEF

	} else {
	    debug      = btoi (clgetb ("debug"))
	    verbose    = btoi (clgetb ("verbose"))
	    gkiunits   = btoi (clgetb ("gkiunits"))

	    # scale precedence: calcomp.par->metacode->graphcap->compile_time
	    g_xtask_scale = clgetr ("xscale")
	    if (!IS_INDEF (g_xtask_scale))
		g_xndcto_p = g_xtask_scale
	    g_ytask_scale = clgetr ("yscale")
	    if (!IS_INDEF (g_ytask_scale))
		g_yndcto_p = g_ytask_scale

	    # Get the quality parameter for the text generator.
	    call clgstr ("txquality", txquality, SZ_TXQUALITY)
	    switch (txquality[1]) {
	    case 'l':
		g_txquality = GT_LOW
	    case 'm':
		g_txquality = GT_MEDIUM
	    case 'h':
		g_txquality = GT_HIGH
	    default:
		g_txquality = 0			# .par default is "normal"
	    }

	    # Method of line width implementation:
	    g_lwtype    = clgetc ("lwtype")

	    # The overrides:
	    g_ltover    = clgetb ("ltover")
	    g_lwover    = clgetb ("lwover")
	    g_lcover    = clgetb ("lcover")

	    # Plotter line type, width control:
	    g_dashlen   = clgetr ("dashlen")
	    g_gaplen    = clgetr ("gaplen")
	    g_plwsep    = clgetr ("plwsep")
	}

	# Open the graphics kernel.
	call ccp_open (Memc[devname], dev)
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
	call ccp_close()
	call clpcls (list)
	call sfree (sp)
end
