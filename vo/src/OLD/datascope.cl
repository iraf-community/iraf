#{  DATASCOPE --  Toy DataScope task that returns a table of data services
#   and the number of results that can be expected for each given either an
#   input image or object name.  For image input, the WCS footprint is computed
#   and used as the search parameters;  For an object name the coordinates are
#   resolved and the search radius specified by the user is used.
#
#   Example:  Query a specific set of services, finding how many hits would
#		be returned against the dev$wpix test image
#
#	cl> datascope dev$wpix service="GSC2.2,USNO-B1,DSS2"
#	#Num  ShortName         Count   IVO Identifier
#	#---  ---------         -----   --------------
#	   1   GSC2.2              50   ivo://archive.stsci.edu/gsc/gsc2.2
#	   2   USNO-B1            344   ivo://fs.usno/cat/usnob
#	   3   DSS2                 1   ivo://archive.stsci.edu/dss/dss2
#
#   REQUIRES:  NVO package and VO-CL builtin functions.
# 
#   M. Fitzpatrick, Aug 2006


procedure datascope (what)

string	what			{ prompt = "Object Name or Image"	   }
string	service = "cone"	{ prompt = "Service Type or list"	   }
string	bandpass = "optical"	{ prompt = "Bandpass of search"		   }
real	sr = 0.25		{ prompt = "search radius"		   }

bool	filter = yes		{ prompt = "Filter results?"		   }
bool	verbose = yes		{ prompt = "Verbose output?"		   }
int	status = 0		{ prompt = "Service status code"	   }

begin
	string  url, tname, stype, this, bpass
	string  sname, ivorn, regResults, junk, svctyp
	string  zresp, eresp
	real	ra, dec, size
	bool    verb, filt
	int	dal, index, count, lnum

	this  = what
	bpass = bandpass
	verb  = verbose
	stype = service
	filt  = filter

	if (stype == "catalog") {
	    stype = "cone"
	} else if (stype == "image") {
	    stype = "siap"
	}
	;

	if (imaccess (this)) {
	    iferr { wcsinfo (this) } then {
		error (0, "Cannot determine image coords for `"//this//"'")
	    } else {
	        ra  = wcsinfo.midx
	        dec = wcsinfo.midy
	        size = max (wcsinfo.width, wcsinfo.height) / 60.0
	    }

	} else { 
	    # Assume it's an object name, resolve to coords.
	    sesame (this)
	    if (sesame.status < 0)
		error (0, "Cannot resolve object `"//this//"'")
	    ra   = sesame.ra
	    dec  = sesame.dec
	    size = sr
	}

	# Initialize
	zresp = ""
	eresp = ""

	# Now do a Registry query for the given service type and bandpass.
	regResults = mktemp ("tmp$reg")
	if (stype == "all") {
	    s1 = mktemp ("tmp$reg")
	    print (stype) | translit ("STDIN",",","\n",del-, > s1)
	    list = s1
	    i = 1

	    # Convert the list of specific services to a table we use below to
	    # query each.
	    while (fscan (list, s2) != EOF) {
		printf ("%4d  %s\n", i, 
		    regResolver (s2,"","ShortName,ServiceType,Identifier"), 
			>> regResults)
		i = i + 1
	    }
	    delete (s1, verify-)

	} else {
	    registry ("", svc=stype, bandpass=bpass, verb-, header-, 
		>& regResults)
	}


	if (verbose) {
	    count (regResults) | scan (i)
	    printf ("#\n")
	    printf ("#  Search Terms:  RA=%H  Dec=%h  SR=%.3f\n", ra, dec, size)
	    printf ("#  Service Type:  %s\n", stype)
	    printf ("#  Total Querys:  %d\n", i)
	    printf ("#\n#\n")
	    printf ("#Num  ShortName         Count   IVO Identifier\n")
	    printf ("#---  ---------         -----   --------------\n")
	}

	# Loop over the registry and query by service type.
	list = regResults
	lnum = 1
	while (fscan (list, index, sname, svctyp, ivorn) != EOF) {

	    count = 0
	    if (substr (svctyp,1,4) == "CONE")
	        count = dalRecordCount (\
			    dalConeSvc(\
				regResolver(ivorn,"cone"), ra, dec, size))
	    else if (substr (svctyp,1,4) == "SIAP")
	        count = dalRecordCount (\
			    dalSiapSvc(\
				regResolver(ivorn,"siap"), ra, dec, size, size))

	    if (count == 0)
		zresp = zresp // sname // ","
	    if (!filter) {
	        printf ("%4d   %-14.14s  ", lnum, sname)
                if (count < 0) {
                    printf (" ERROR   %s\n", ivorn)
		    eresp = eresp // sname // ","
                } else
                    printf ("%6d   %s\n", count, ivorn)
		lnum = lnum + 1
	    } else if (filter && count > 0) {
	        printf ("%4d   %-14.14s  %6d   %s\n", lnum,sname,count,ivorn)
		lnum = lnum + 1
	    } else if (count < 0)
		eresp = eresp // sname // ","
	}

	if (verbose) {
	    printf ("#\n")
	    printf ("#  Zero Values: ") ; prettystr(zresp,start=15,comment+)
	    printf ("#  Err Returns: ") ; prettystr(eresp,start=15,comment+)
	}

	# Clean up.
	delete (regResults, verify-)
end
