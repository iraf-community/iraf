#{  SSGET -- Get a single SDSS spectrum by ID.

procedure ssget (id, oname)

string	id 			{ prompt = "Image ID"	   		   }
string	oname 			{ prompt = "Output image"		   }

bool	verbose = no		{ prompt = "Verbose output?"		   }
int	status = 0		{ prompt = "Service status code"	   }

begin
    string  url, base, objid, out, inid
    string  keyw, val, junk, ns, tfile, res
    string  ID, NAME, DESCR, RA, DEC, Z, ZERR, ZCONF, CLASS
    int     npoints, nlines, idx


    # Set the environment.
    reset clobber = yes
    reset imclobber = yes

    # Get params to local variables.  Escape the spectrum identifier for
    # URL.
    inid = id
    idx  = stridx ("\\", inid)
    if (idx > 0) {
	s1 = inid
	inid = substr (s1, 1, idx-1) // substr (s1, idx+1, strlen(s1)) 
    }
    print (inid) | \
	sed("-e","s:\\\://:%3a%2f%2f:g","-e","s:/:%2f:g",
	    "-e","s:\#:%23:g") | scan (objid)

    out   = oname
    verb  = verbose

    if (verb)
	print (inid // " -> " // out)


    # Build the URL for the query
    base = "http://voservices.net/spectrum/search_details.aspx?"
#   url = base // "format=ascii&id=ivo%3a%2f%2fjhu%2fsdss%2fdr4%23" // objid
    url = base // "format=ascii&id=" // objid

    tfile = osfn (mktemp ("tmp$tfile"))

    # Do the query and save the results to a CSV table.
    res = osfn (mktemp ("tmp$ss"))
    rawcaller (url, otype="raw", output="STDOUT", >& res)

    # Check whether we got a reasonable result.  
    count (res) | scan (nlines)
    if (nlines < 5) {
        if (verb)
    	    print ("# No results found.")
        return
    }


    # Get some header keywords
    list = res
    ns = fscan (list, junk, keyw, junk, val)      # banner
    ns = fscan (list, junk, keyw, junk, ID)       # ID
    ns = fscan (list, junk, keyw, junk, NAME)     # ID
    ns = fscan (list, junk, keyw, junk, DESCR)    # ID
    ns = fscan (list, junk, keyw, junk, RA)       # ID
    ns = fscan (list, junk, keyw, junk, DEC)      # ID
    ns = fscan (list, junk, keyw, junk, val)      # origin
    ns = fscan (list, junk, keyw, junk, Z)        # ID
    ns = fscan (list, junk, keyw, junk, ZERR)     # ID
    ns = fscan (list, junk, keyw, junk, ZCONF)    # ID
    ns = fscan (list, junk, keyw, junk, CLASS)    # ID

    # Produce the text file we'll use as input to RSPECTEXT.
    fields (res, "1,2", > tfile)

    # Create the spectrum
    rspectext (res, out, title=NAME, flux+, dtype="nonlinear")

    # Update the header with certain keywords.
    hedit (out, "OBJECT", NAME, add+, verify-, update+, show-)
    hedit (out, "DESCR", DESCR, add+, verify-, update+, show-)
    hedit (out, "CLASS", CLASS, add+, verify-, update+, show-)
    hedit (out, "RA", RA, add+, verify-, update+, show-)
    hedit (out, "DEC", DEC, add+, verify-, update+, show-)
    hedit (out, "Z", Z, add+, verify-, update+, show-)
    hedit (out, "ZERR", ZERR, add+, verify-, update+, show-)
    hedit (out, "ZCONF", ZCONF, add+, verify-, update+, show-)


    # Clean up.
    delete (tfile, verify-)
    delete (res, verify-)
end
