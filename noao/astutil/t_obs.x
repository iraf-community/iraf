include	<error.h>

define	COMMANDS	"|set|list|images|"
define	SET		1	# Set observatory task parameters
define	LIST		2	# List default observatory data
define	IMAGES		3	# List observatory data for images

# T_OBSERVATORY -- Set/list parameters from the observatory database.

procedure t_observatory ()

pointer	list			# Image list
pointer	observatory		# Default observatory
int	verbose			# Verbose?

int	cmd, clgwrd(), btoi(), imtgetim()
pointer	sp, image, obs, im, imtopenp(), obsvopen(), immap()
bool	new, header, clgetb()
double	dval, obsgetd()
errchk	obsvopen, obsimopen

begin
	call smark (sp)
	call salloc (observatory, SZ_FNAME, TY_CHAR)

	cmd = clgwrd ("command", Memc[observatory], SZ_FNAME, COMMANDS)
	call clgstr ("obsid", Memc[observatory], SZ_FNAME)
	verbose = btoi (clgetb ("verbose"))
	switch (cmd) {
	case SET: # Set default observatory and observatory parameters
	    obs = obsvopen (Memc[observatory], verbose)
	    if (obs != NULL) {
		# List
		call obslog (obs, "", "", STDOUT)
		call obsinfo (obs, STDOUT)

		# Fill in parameter file.
		call obsgstr (obs, "observatory", Memc[observatory], SZ_FNAME)
		call clpstr ("observatory", Memc[observatory])
		call obsgstr (obs, "name", Memc[observatory], SZ_FNAME)
		call clpstr ("name", Memc[observatory])
		iferr (dval = obsgetd (obs, "longitude"))
		    dval = INDEFD
		call clputd ("longitude", dval) 
		iferr (dval = obsgetd (obs, "latitude"))
		    dval = INDEFD
		call clputd ("latitude", dval) 
		iferr (dval = obsgetd (obs, "altitude"))
		    dval = INDEFD
		call clputd ("altitude", dval) 
		iferr (dval = obsgetd (obs, "timezone"))
		    dval = INDEFD
		call clputd ("timezone", dval) 
		call obsclose (obs)
	    }

	case LIST: # List observatory parameters for specified observatory
	    obs = obsvopen (Memc[observatory], verbose)
	    if (obs != NULL) {
		# List
		call obslog (obs, "", "", STDOUT)
		call obsinfo (obs, STDOUT)
		call obsclose (obs)
	    }

	case IMAGES: # List observatory parameters for a list of images
	    call salloc (image, SZ_FNAME, TY_CHAR)

	    list = imtopenp ("images")
	    obs = NULL
	    while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		# Get image observatory
		iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    next
		}
		call obsimopen (obs, im, Memc[observatory], verbose, new,header)
		call imunmap (im)
		if (obs == NULL)
		    next

		# Print observatory info
		if (new) {
		    call obslog (obs, "", "", STDOUT)
		    call obsinfo (obs, STDOUT)
		    call printf ("\tImages:\t%s")
			call pargstr (Memc[image])
		} else {
		    call printf ("\t\t%s")
			call pargstr (Memc[image])
		}
		if (header)
		    call printf (" (OBSERVAT keyword)\n")
		else
		    call printf (" (default observatory)\n")
	    }
	    call imtclose (list)
	    call obsclose (obs)
	}

	call sfree (sp)
end
