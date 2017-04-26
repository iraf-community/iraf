#{  DATASCOPE --  Toy DataScope task that returns a table of data services
#   and the number of results that can be expected for each given either an
#   input image or object name.  For image input, the WCS footprint is computed
#   and used as the search parameters;  For an object name the coordinates are
#   resolved and the search radius specified by the user is used.
#
#   Examples:  
#   1)  Query a specific set of services, finding how many hits would
#		be returned against the dev$wpix test image
#
#	cl> datascope dev$wpix service="GSC2.2,USNO-B1,DSS2"
#	#Num  ShortName         Count   IVO Identifier
#	#---  ---------         -----   --------------
#	   1   GSC2.2              50   ivo://archive.stsci.edu/gsc/gsc2.2
#	   2   USNO-B1            344   ivo://fs.usno/cat/usnob
#	   3   DSS2                 1   ivo://archive.stsci.edu/dss/dss2
#
#   2)  Query all Cone Services offering Infrared data against our image
#
#	cl> datascope dev$wpis service="cone" bandpass="infrared"
#
#   REQUIRES:  NVO package and VO-CL builtin functions.
# 
#   M. Fitzpatrick, Aug 2006


procedure datascope (what)

string	what			{ prompt = "Object Name or Image"	   }
string	service = "cone"	{ prompt = "Service Type or list"	   }
string	bandpass = "optical"	{ prompt = "Bandpass of search"		   }
real	sr = 0.25		{ prompt = "search radius"		   }

bool	verbose = yes		{ prompt = "Verbose output?"		   }
int	status = 0		{ prompt = "Service status code"	   }

begin
	string  url, tname, stype, this, bpass
	string  sname, ivorn, regResults, junk, svctyp
	real	ra, dec, size
	bool    verb
	int	dal, index, count

	this  = what
	bpass = bandpass
	verb  = verbose
	stype = service

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


	# Now do a Registry query for the given service type and bandpass.
	regResults = mktemp ("tmp$reg")
	if (stype == "cone" || stype == "siap") {
	    registry ("", svc=stype, bandpass=bpass, verb-, header-, 
		>& regResults)

	} else {
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
	}


	if (verbose) {
	    printf ("#\n")
	    printf ("#  Search Terms:  RA=%H  Dec=%h  SR=%.3f\n", ra, dec, size)
	    printf ("#  Service Type:  %s\n", stype)
	    printf ("#\n#\n")
	    printf ("#Num  ShortName         Count   IVO Identifier\n")
	    printf ("#---  ---------         -----   --------------\n")
	}

	# Loop over the registry and query by service type.
	list = regResults
	while (fscan (list, index, sname, svctyp, ivorn) != EOF) {
	    printf ("%4d   %-14.14s  ", index, sname)

	    if (substr (svctyp,1,4) == "CONE")
	        count = dalRecordCount (\
			    dalConeSvc(\
				regResolver(ivorn), ra, dec, size))
	    else if (substr (svctyp,1,4) == "SIAP")
	        count = dalRecordCount (\
			    dalSiapSvc(\
				regResolver(ivorn), ra, dec, size, size))

	    if (count < 0)
	        printf (" Error   %s\n", ivorn)
	    else
	        printf ("%6d   %s\n", count, ivorn)
	}

	# Clean up.
	delete (regResults, verify-)
end
