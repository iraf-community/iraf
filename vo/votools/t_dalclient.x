#
#  DALCLIENT -- Call a VO DAL service and return the raw VOTable result.

include <ctype.h>


define	SZ_URL		512
define	SZ_RESULT	16777216	# allow up to 16M of results


procedure t_dalclient ()

pointer	sp, result, ip
pointer	fname, otype, svctype, svcurl, fmt
double	ra, dec, sr, rasz, decsz
int	out, reslen

double	clgetd()
bool	streq()

int	open()

int	vx_initVOClient()
int	vx_conecaller()		# returns the length of the result
int	vx_siapcaller()		# returns the length of the result
int	vx_rawurl()		# returns the length of the result

begin
	# Allocate local storage.
	call smark (sp)
	call salloc (fmt, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (otype, SZ_FNAME, TY_CHAR)
	call salloc (svctype, SZ_FNAME, TY_CHAR)
	call salloc (svcurl, SZ_URL, TY_CHAR)

	# Clear string arrays.
	call aclrs (Memc[fmt], SZ_FNAME)		
	call aclrs (Memc[fname], SZ_FNAME)
	call aclrs (Memc[otype], SZ_FNAME)
	call aclrs (Memc[svctype], SZ_FNAME)
	call aclrs (Memc[svcurl], SZ_URL)


	# Get the task parameters.
	call clgstr ("svc_url", Memc[svcurl], SZ_URL)     # service URL
	call clgstr ("svc_type", Memc[svctype], SZ_FNAME) # service type
	call clgstr ("output", Memc[fname], SZ_FNAME)	  # output fname|STDOUT


	# We're a hidden task so we should only be called with a few
	# known types.  Make the rest of the parameter queries depending
	# on the type of service we're calling.

	if (streq (Memc[svctype], "cone")) {
	    ra = clgetd ("ra")				# position (decimal deg)
	    dec = clgetd ("dec")
	    sr = clgetd ("sr")

	} else if (streq ( Memc[svctype], "siap")) {
	    ra = clgetd ("ra")				# position (decimal deg)
	    dec = clgetd ("dec")
	    rasz = clgetd ("xsize")
	    decsz = clgetd ("ysize")
	    call clgstr ("imfmt", Memc[fmt], SZ_FNAME)

	} else if (streq ( Memc[svctype], "raw")) {
	    ;	# no-op, got the service url above

	} else {
	    call eprintf ("Invalid service type '%s'\n")
		call pargstr (svctype)
	    call sfree (sp)
	    return
	}
	call clgstr ("otype", Memc[otype], SZ_FNAME)	# "csv" or "votable"


	# Allocate space for the result after we've verified the parameters.
	call calloc (result, SZ_RESULT, TY_CHAR)

	# Now call the requested service.
	if (streq ("cone", Memc[svctype])) {
	    reslen = vx_coneCaller (Memc[svcurl], ra, dec, sr, Memc[otype], 
		Memc[result], SZ_RESULT)

	} else if (streq ("siap", Memc[svctype])) {
	    reslen = vx_siapCaller (Memc[svcurl], ra, dec, rasz, decsz, 
		Memc[fmt], Memc[otype], Memc[result], SZ_RESULT)

	} else if (streq ("raw", Memc[svctype])) {
            # Initialize the VO Client interface.
            if (vx_initVOClient("") == ERR) {               
                call clputi ("status", ERR)
                call error (0, "Error initializing VO Client")
                return
            }

	    reslen = vx_rawurl (Memc[svcurl], Memc[result], SZ_RESULT)

            call vx_closeVOClient (0)
	}


	# Skip leading whitespace and newlines.
	for (ip = result; IS_WHITE(Memc[ip]) || Memc[ip] == '\n'; ) {
	    ip = ip + 1
	    reslen = reslen - 1
	}


	# Print the results to the screen.
	if (reslen > 1) {
	    if (streq (Memc[fname], "STDOUT")) {
	        call write (STDOUT, Memc[ip], reslen)
		call printf ("\n")
	    } else {
                out = open (Memc[fname], NEW_FILE, TEXT_FILE)
	        call write (out, Memc[ip], reslen)
		call fprintf (out, "\n")
		call close (out)
	    }

	    if (reslen >= SZ_RESULT) {
		call eprintf ("\n\nResults possibly truncated....\n\n")
	        call clputi ("status", ERR)
	    } else {
	        call clputi ("status", OK)
	    }

	}  else {
	    call eprintf ("No results from service\n")
	    call clputi ("status", ERR)
	}
	call clputi ("reslen", reslen)

	# Clean up.
	call mfree (result, TY_CHAR)
	call sfree (sp)
end
