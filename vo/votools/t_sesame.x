#
# SESAME -- Call the Sesame web service to resovle an object name to 
# (J2000) Equatorial coordinates using the VO Client interface.


define	NRANGES		64

procedure t_sesame ()

char	target[SZ_FNAME], root[SZ_FNAME], tname[SZ_FNAME], range_str[SZ_FNAME]
char	pos[SZ_LINE], otype[SZ_LINE], ip, op
double	ra, dec, ra_err, dec_err
int	sr, len, ntargets, nobjs, nvalues, number, list, status
int	ranges[2,NRANGES]
bool	verbose, full

int	vx_initVOClient()
int	vx_nameresolver(), vx_resolverpos(), vx_resolverotype()
double	vx_resolverra(), vx_resolverdec()
double	vx_errresolverra(), vx_errresolverdec()

int	fntopnb(), fntgfnb(), fntlenb()
int	decode_ranges(), get_next_number()
bool	clgetb(), streq()

begin
	status = OK

	# Get the target name and verbose parameter.
	call clgstr ("target", target, SZ_FNAME)
	call clgstr ("range", range_str, SZ_FNAME)
	verbose = clgetb ("verbose")
	full = clgetb ("long")


	# Initialize the VO Client interface.
	if (vx_initVOClient("") == ERR) {		
	    call clputi ("status", ERR)
	    call error (0, "Error initializing VO Client")
	    return
	}

	# Encode spaces in the object name.
	for (ip=1; target[ip] != EOS; ip=ip+1) {
	    if (target[ip] == ' ')
		target[ip] = '+'
	}

	# Open the target list.
	list = fntopnb (target, NO)
	nobjs = fntlenb (list)
	ntargets = nobjs

	# Loop over the object list.
	while (nobjs > 0) {

	    # Get the target name from the list.
	    if (fntgfnb (list, root, SZ_FNAME) == EOF)
		break;

	    # Initialize the range string decoding.
	    nvalues = 0
	    if (range_str[1] != EOS) {
		if (decode_ranges (range_str, ranges, NRANGES, nvalues)==ERR) {
		    call eprintf ("Error decoding range string '%s'\n")
			call pargstr (range_str)
		    break
		}
	    }

	    # Loop over the range string.
	    while (nvalues == 0 || get_next_number (ranges, number) != EOF) {

		call strcpy (root, tname, SZ_FNAME)		# single target
		if (nvalues > 0) {				# target list
		    call sprintf (tname, SZ_FNAME, "%s%d")
			call pargstr (root)
			call pargi (number)
		}

	        # Call the service and get the results.
	        sr  = vx_nameresolver (tname)

	        len = vx_resolverpos (sr, pos, SZ_LINE)
	        ra  = vx_resolverra  (sr)
	        dec = vx_resolverdec (sr)
	        ra_err  = vx_errresolverra  (sr) / 1000.   # convert to arcsec
	        dec_err = vx_errresolverdec (sr) / 1000.
	        len = vx_resolverotype (sr, otype, SZ_LINE)

	        if (streq (pos, "nul") && streq (otype, "nul"))
		    status = ERR

	        # If we're not being quiet, print the RA and Dec so they may be
	        # easily scanned into variables from a script.
	        if (verbose) {
	            if (status == ERR)  		# no resolution
		        call strcpy  ("INDEF INDEF", pos, SZ_FNAME)

	            if (full) {
	                call printf ("%s %s %.3f %.3f %s\n")
	                    call pargstr (tname)
	                    call pargstr (pos)
		            call pargd (ra_err)
		            call pargd (dec_err)
		            call pargstr (otype)
	            } else {
	                call printf ("%s\n")
	                    call pargstr (pos)
		    }
	        }

		if (nvalues == 0)
		    break
	    }
	    ntargets = ntargets + nvalues

	    nobjs = nobjs - 1
	}

	# Save the results to the parameter file in any case.
	if (ntargets == 1) {
	    call clpstr ("pos", pos)
	    call clputd ("ra", ra)
	    call clputd ("dec", dec)
	    call clputd ("ra_err", ra_err)
	    call clputd ("dec_err", dec_err)
	    call clpstr ("otype", otype)
	} else
	    status = ERR

	call clputi ("status", status)

	call vx_closeVOClient (0)
end
