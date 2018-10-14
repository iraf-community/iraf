# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imset.h>
include <mach.h>

task urlget	= t_urlget



# URLGET -- Do an HTTP GET of a URL to the named file.

procedure t_urlget ()

char	url[SZ_PATHNAME], fname[SZ_PATHNAME], extn[SZ_PATHNAME]
char	cache[SZ_PATHNAME], lfname[SZ_PATHNAME]
int	nread
bool	use_cache, verbose

int	url_get()
bool	fcaccess()

begin
	# Get the parameters
	call clgstr ("url", url, SZ_PATHNAME)
	
	call url_to_name (url, fname, SZ_PATHNAME)
	call strcpy ("", extn, SZ_PATHNAME)
	call strcpy ("/tmp/cache/", cache, SZ_PATHNAME)
	verbose = true
	use_cache = false


	# Tell them what we're doing.
	if (verbose) {
	    call printf ("%s -> %s\n")
		call pargstr (url)
		call pargstr (fname)
	    call flush (STDOUT)
	}

	# Retrieve the URL.
	if (use_cache) {
	    call aclrc (lfname, SZ_FNAME);

	    if (fcaccess (cache, url, "fits")) {
	        call fcname (cache, url, "f", lfname, SZ_PATHNAME)
		if (extn[1] != EOS) {
		    # Add an extension to the cached file.
		    call strcat (".", lfname, SZ_PATHNAME)
		    call strcat (extn, lfname, SZ_PATHNAME)
		}
	    } else {
	        # Add it to the cache, also handles the download.
	        call fcadd (cache, url, extn, lfname, SZ_PATHNAME)
	    }
	    call fcopy (lfname, fname)

	} else {
	    # Not in cache, or not using the cache, so force the download.
	    nread = url_get (url, fname, NULL)
	}
end


# URL_TO_NAME -- Generate a filename from a URL.

procedure url_to_name (url, name, maxch)

char	url[ARB]				#i URL being accessed
char	name[ARB]				#o output name
int	maxch					#i max size of output name

int	ip, strlen()
char	ch

begin
	ip = strlen (url)
	while (ip > 1) {
	    ch = url[ip]
	    if (ch == '/' || ch == '?' || ch == '&' || ch == ';' || ch == '=') {
		call strcpy (url[ip+1], name, maxch)
	        return
	    }
	    ip = ip - 1
	}

	call strcpy (url[ip], name, maxch)
end
