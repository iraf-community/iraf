#{  VOIMAGE -- Call a SIAP Search service.

procedure voimage (svc_url, ra, dec, rasz, decsz)

string	svc_url			{ prompt = "Service URL"		}
real	ra			{ prompt = "RA of search"		}
real	dec			{ prompt = "Dec of search"		}
real	rasz			{ prompt = "RA size"			}
real	decsz			{ prompt = "Dec size"			}

string  insystem = "icrs"       { prompt = "Input coordinate system"    }
string  image    = ""		{ prompt = "Image Name"			}
string	fmt      = "image/fits"	{ prompt = "Image format type"		}
string	output   = "STDOUT"	{ prompt = "Output filename"		}
string	otype    = "ascii"	{ prompt = "Output format",
				    min="ascii|csv|votable|fits"	} 
bool    verbose  = yes          { prompt = "Verbose output?"            }
int	status   = 0		{ prompt = "Service status code"	}

begin
	string  url, oname, tname, ctype, lfmt, isys, img
	real	lra, ldec, lrasz, ldecsz


	url    = svc_url
	if (substr(url,1,4) != "http") {
	    url = regResolver (url,"siap")
            if (url == "INDEF") {
                error (0, "Cannot resolve '" // svc_url // 
		   "' to known Cone service,")
	    }
        }

        img   = image
        if (imaccess (img)) {
            iferr { wcsinfo (img) } then {
                error (0, "Cannot determine image coords for `"//img//"'")
            } else {
                lra  = wcsinfo.midx
                ldec = wcsinfo.midy
                lrasz = wcsinfo.width / 60.0
		ldecsz = wcsinfo.height / 60.0
            }

        } else {
            # No image specified, use the parameters.
            lra    = ra
            ldec   = dec
            lrasz  = rasz
            ldecsz = decsz
        }
	ctype  = otype
	oname  = output
	lfmt   = fmt
        isys  = insystem
        verb  = verbose

        # If we're outputing a VOTable we probably only want the raw
        # XML, so suppress the status messages.
        if (oname == "plastic" || oname == "topcat" || oname == "aladin")
            ctype = "votable"
        if (ctype == "votable") 
            verb = no

        if (isys == "degrees") {
            ;                                   # No transformation
        } else if (isys == "icrs") {
            lra = lra * 15.0                    # convert to degrees
            ldec = ldec
        } else if (isys != "icrs") {
						# Transform coordinates
            print (lra // " " // ldec) | \	
                skyctran ("STDIN","STDOUT",isys,"icrs",
                    olngu="deg", olatu="deg", olngf="%f", olatf="%f") | \
                fields ("STDIN","1,2",lines="9") | \
                scan (x, y)
            if (verb) {
                print ("#")
                print ("#   Input coords: ("//lra//","//ldec//") ("//isys//")")
                print ("#  Output coords: ("//x//","//y//") (ICRS)")
                print ("#")
            }
            lra = x
            ldec = y
        }

	# Create a temporary output name.
	tname = mktemp ("tmp$siap") // ".xml"

	# Call the Cone service via the DALCLIENT generic task.  DALCLIENT
        # expects decimal degrees as the coordinates, we should have done the
        # required transformation above.
	dalclient (url, "siap", ra=lra, dec=ldec, ra_size=lrasz, 
	    dec_size=ldecsz, otype="votable", output=tname)

        # Output the table.
        if (dalclient.status >= 0) {
	    tabout (tname, oname, ctype)

            if (tabout.nrows == 0) {
                printf ("\nNo results found\n");
            }
	}

	delete (tname, verify-, >& "dev$null") 			# clean up
end
