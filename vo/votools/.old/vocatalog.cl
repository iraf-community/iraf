#{  VOCATALOG -- Call a Cone Search service.  Input coords may be in any of
#   the supported systems and will be converted to ICRS degrees for the query
#   automatically.

procedure vocatalog (service, ra, dec, sr)

string	service			{ prompt = "Service URL"		}
real	ra			{ prompt = "RA of search"		}
real	dec			{ prompt = "Dec of search"		}
real	sr			{ prompt = "search radius"		}

string	insystem = "icrs"	{ prompt = "Input coordinate system"	}
string	image    = ""		{ prompt = "Image Name"			}
string	output   = "STDOUT"	{ prompt = "Output filename"		}
string	otype    = "ascii"	{ prompt = "Output format"		}
bool	verbose  = yes		{ prompt = "Verbose output?"		}
int	status   = 0		{ prompt = "Service status code"	}

begin
	string  svc_url, svc_name, svc_id, fields
	string  oname, tname, ctype, isys, img
	real	lra, ldec, lsr
	bool    verb
	int	nres


	svc_name  = ""
	svc_id    = ""
	svc_url   = service
	nres      = 1
	if (substr(svc_url,1,4) != "http") {
	    fields = "ServiceURL,ShortName,Identifier,Title"
	    print (regResolver(svc_url,"",fields)) | \
		scan(svc_url,svc_name,svc_id,line)
	    nres = nresolved()
	    if (svc_url == "INDEF") 
		error (0, "Cannot resolve '" // svc_url // "' to service")
	}

	img   = image
	isys  = insystem
	if (imaccess (img)) {
            iferr { wcsinfo (img) } then {
                error (0, "Cannot determine image coords for `"//img//"'")
            } else {
                lra  = wcsinfo.midx
                ldec = wcsinfo.midy
                lsr = max (wcsinfo.width, wcsinfo.height) / 60.0
		isys = "degrees"
            }
	} else {
	    # No image specified, use the parameters.
	    lra   = ra
	    ldec  = dec
	    lsr   = sr
	}
	ctype = otype
	oname = output
	verb  = verbose

	# If we're outputing a VOTable we probably only want the raw
	# XML, so suppress the status messages.
	if (oname == "plastic" || oname == "topcat" || oname == "aladin")
	    ctype = "votable"
	if (ctype == "votable")
	    verb = no

	if (verb) {
	    print ("#")
	    printf ("#        ShortName: " // svc_name)
	    if (nres > 1) 
	        printf ("  (%d possible matches)\n", nres)
	    else
	        printf ("\n")
	    print ("#       ServiceURL: " // svc_url)
	    print ("#       Identifier: " // svc_id)
	    print ("#            Title: " // line)
	    if (strstr(isys,"degrees") > 0)
	        printf ("#     Input coords: (%H,%h) (%s)\n", lra, ldec, isys)
	    else
	        printf ("#     Input coords: (%h,%h) (%s)\n", lra, ldec, isys)
	}

	if (strstr(isys,"degrees") > 0) {
	    ;					# No transformation
	} else if (isys == "icrs") {
	    lra = lra * 15.0			# convert to degrees
	    ldec = ldec
	} else if (isys != "icrs") {
						# Transform coords
	    print (lra // " " // ldec) | \
		skyctran ("STDIN","STDOUT",isys,"icrs", transform+,
		    olngu="deg", olatu="deg", olngf="%f", olatf="%f") | \
		        fields ("STDIN","1,2",lines="9") | \
		        scan (x, y)
	    lra = x
	    ldec = y
	}
	if (verb)
	    print ("#     Query coords: ("//lra//","//ldec//") ("//lsr//")")

	# Create a temporary output name.
	tname = mktemp ("tmp$cone") // ".xml"

	# Call the Cone service via the DALCLIENT generic task.  DALCLIENT
	# expects decimal degrees as the coordinates, we should have done the
	# required transformation above.
	dalclient.ra=lra
	dalclient.dec=ldec
	dalclient.sr=lsr
	dalclient (svc_url, "cone", otype="votable", output=tname)

	if (verb) print ("#")

	# Output the table.
	if (dalclient.status >= 0) {
	    votcopy (tname, oname, ctype)
	}

	if (access(tname)) 
	    delete (tname, verify-, >& "dev$null")
end
