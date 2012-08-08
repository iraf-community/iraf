#{  SSQUERY -- Query the SDSS Spectrum Services for a table of spectra.
#   Spectra may be searched for either spatially using the footprint of
#   an existing image, a search radius around a named object, or by explicit
#   coordinates.  Additionally, spectra within a specific redshift range
#   may be searched by setting the zlow/zhigh parameters.
#
#   In all cases the complete query result is returned.  To filter the 
#   the results further a task such as TSELECT may be used on the table.

procedure ssquery ()

string	image = ""		{ prompt = "Image (Footprint) Name"	   }

string	object = ""		{ prompt = "Object Name"	   	   }

real	ra = INDEF		{ prompt = "RA of query (hours)"	   }
real	dec = INDEF		{ prompt = "Dec of query (deg)"		   }
real	sr = 15.		{ prompt = "Search radius (arcmin)"	   }

real	zlow = INDEF		{ prompt = "Low redshift filter"	   }
real	zhigh = INDEF		{ prompt = "High redshift filter"	   }

string	output = "STDOUT"	{ prompt = "Output table"		   }
int	maxrecords = 100	{ prompt = "Max records to return"	   }
bool	incsky = no		{ prompt = "Include Sky spectra?"	   }

bool	verbose = yes		{ prompt = "Verbose output?"		   }
int	status = 0		{ prompt = "Service status code"	   }

begin
    string  url, base, colID, tname, stype, img, bpass
    string  res, csv, ofile, tfile, obj
    string  oname, oid, oclass, osclass, release
    real    ora, odec, ored
    real    lra, ldec, lsr, lzlo, lzhi
    bool    verb, zsearch, zfilter, use_sky
    int	    maxvals, nlines, stat


    # Set the environment.
    reset clobber = yes
    reset imclobber = yes

    zsearch = no
    zfilter = no

    # Get params to local variables.
    img     = image
    obj     = object
    ofile   = output
    verb    = verbose
    lra     = ra
    ldec    = dec
    lsr     = sr
    lzlo    = zlow
    lzhi    = zhigh
    maxvals = maxrecords
    use_sky = incsky

    release = "dr4"
    stat    = 0

    # Determine the type of search from the parameters.
    if (img != "") {
        if (imaccess (img)) {
            iferr { wcsinfo (img) } then {
    	        error (0, "Cannot determine image coords for `"//img//"'")
            } else {
                lra  = wcsinfo.midx
                ldec = wcsinfo.midy
                lsr  = max (wcsinfo.width, wcsinfo.height) #/ 60.0
            }
        } else
    	    error (0, "Cannot access image `"//img// "'")

    } else if (obj != "") {
        # Assume it's an object name, resolve to coords.
        sesame (obj)
        if (sesame.status < 0)
            error (0, "Cannot resolve object `"//obj//"'")
        lra    = sesame.ra	# SESAME gives us decimale degrees ....
        ldec   = sesame.dec
        lsr    = sr
 	stat   = sesame.status

    } else if (! isindef(lra) && ! isindef(ldec)) {
        # No image specified, use parameters we assume are in hms/dms & arcmin
        lra   = (lra * 15.0)	# hours to degrees
        ldec  = ldec		# degrees
        lsr   = lsr		# arcmin
	zsearch = no
    }

    if (!isindef(lzlo) || !isindef(lzhi)) {
	if (! isindef(lra) && ! isindef(ldec))
	    zfilter = yes
	else
	    zsearch = yes
    } else if (isindef(lra) || isindef(ldec))
        error (0, "Must specify search by image/object/position/redshift")


    # Initialize
    base = "http://voservices.net/"		# Base URL
    colID = "ivo%3a%2f%2fjhu%2fsdss%2f"		# Base of Collection ID
    colID = colID // release			# Full ID

    # Build the URL for the query
    if (zsearch) {
        base = base // "spectrum/ws_v2_5/search.asmx/FindSpectraRedshift?"
        url = base // "userGuid=&returnPoints=false&"
        url = url  // "collectionId=" // colID // "&"
	if (isindef(lzlo)) lzlo = 0.0
	if (isindef(lzhi)) lzhi = 9.0
        url = url  // "redshiftFrom=" // lzlo // "&"
        url = url  // "redshiftTo=" // lzhi

    } else {
        base = base // "spectrum/ws_v2_5/search.asmx/FindSpectraCone?"
        url = base // "userGuid=&returnPoints=false&"
        url = url  // "collectionId=" // colID // "&"
        url = url  // "ra="  // lra  // "&"
        url = url  // "dec=" // ldec // "&"
        url = url  // "sr="  // lsr
    }
    #print (url)
    
print (url)


    # Do the query and save the results to a CSV table.
    res = osfn (mktemp ("tmp$stab"))
    csv = osfn (mktemp ("tmp$stab") // ".csv")
    rawcaller (url, otype="raw", output="STDOUT", >& res)

    # Check whether we got a reasonable result.  Rather than try to parse
    # error returns of various forms, check for a "short" file indicating
    # we didn't get what we want.
    count (res) | scan (nlines)
    if (nlines < 5) {
        if (verb) {
    	    print ("# No results found.")
	}
	status = -1
        return
    }

    # Process the returned XML to create a CSV file.
    xsltproc (osfn("nvo$lib/specCone.xsl"), res, >& csv)

    tfile = osfn (mktemp ("tmp$tfile"))

    stilts ("tcopy", "ifmt=csv", "ofmt=ascii", csv, tfile)
    fields (tfile,"1-7") | fields ("STDIN","2-8", > tfile)

    list = tfile
    while (fscan(list,oname,ora,odec,ored,oclass,osclass,oid) != EOF) {
	if (zfilter) {
	    if (!isindef(lzlo) && ored < lzlo)
		next
	    if (!isindef(lzhi) && ored > lzhi)
		next
	}

	# Skip sky spectra?
	if (osclass == "Sky" && use_sky == no) 
	    next

	if (ofile == "STDOUT") {
	    # Need to break this out since redirecting onto STDOUT here
	    # means we can't redirect from the caller.
            printf ("%s %H %h %.6f %-7s %-7s %s\n", 
    	        oname, ora, odec, ored, oclass, osclass, oid)
	} else {
            printf ("%s %H %h %.6f %-7s %-7s %s\n", 
    	        oname, ora, odec, ored, oclass, osclass, oid, >> ofile)
	}
    }
    delete (tfile, verify-)

    # Clean up.
    delete (res, verify-)
    delete (csv, verify-)

    # Reset the parameters so we don't remember values that could confuse
    # us next time.
    image  = ""
    object = ""
    ra     = INDEF
    dec    = INDEF
    zlow   = INDEF
    zhigh  = INDEF
    status = stat
end
