#{  QSTRING -- Build a VO query URL from arguments.

procedure qstring (base, field)

string	base			{ prompt = "Base URL or resource"	}
string	field			{ prompt = "Query image/field"		}

string	type	= "catalog"	{ prompt = "Service type"		}

real    ra       = 0.0 		{ prompt = "RA of field"		}
real    dec      = 0.0   	{ prompt = "Dec of field"		}
real    size     = 0.25  	{ prompt = "Field size"			}

begin
	string  lname, lres, url, tname, largs
	real	lra, ldec, lsize
	int     nread, len


	# Get params to local variables
	lsize  = size
	largs  = "test"

	#  Resolve resource name.
	lres  = base		
	if (lres == "")
	    error (0, "No base URL specified")
	    
	if (substr (lres, 1, 7) == "http://") {
	    url  = lres
	    lname = ""
	} else {
	    lname  = field
	    if (type == "catalog")
	        regdb ("resolve", lres, type="C", >& "dev$null")
	    else
	        regdb ("resolve", lres, >& "dev$null")

	    if (regdb.status == 1)
	        url   = regResolver (lres)	# not found, query the Registry
	    else
	        url   = regdb.url
	}

	#  Make sure the URL ends in a '?' or an '&'.
	len = strlen (url)
	if (substr(url,len,len) != '?' && substr(url,len,len) != '&') {
	    if (strstr ("?", url) == 0) 
		url = url // "?"
	    else
		url = url // "&"
	}

	#  Determine the position information.
	if (imaccess (lname) == no) {
	    # Field is not an image, assume it is an object name.  If we
	    # can resolve the name use that, otherwise use the input params.
            sesame (lname, verbose-)
	    if (sesame.status == 0) {
                ra  = sesame.ra
                dec = sesame.dec
	    }

	    if (type == "catalog")
	        largs = "RA=" // ra // "&DEC=" // dec  // "&SR=" // lsize
	    else
	        largs = "POS=" // ra // "," // dec  // "&SIZE=" // lsize

	} else {
	    # Determine the query params from an image WCS.
	    wcsinfo (lname)
	    if (type == "catalog") {
	        largs = "RA=" // wcsinfo.ra 
	        largs = largs // "&DEC=" // wcsinfo.dec 
	        largs = largs // "&SR=" // wcsinfo.size
	    } else {
	        largs = "POS=" // wcsinfo.ra  // "," // wcsinfo.dec 
	        largs = largs // "&SIZE=" // wcsinfo.size
	    }
	}

	#  Add the RUNID logging param.
	if (vo.runid != "")
	    largs = largs // "&RUNID=" // vo.runid


	#  Printe the derived query string.
	print (url // largs)
end
