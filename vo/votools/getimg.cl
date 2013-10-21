#{  GETIMG -- Call an SIA service URL and return the raw result.

procedure getimg (resource, fields)

string	resource		{ prompt = "Resource name"		}
string	fields			{ prompt = "Query images/fields"	}
string	pos   = ""		{ prompt = "POS string"			}
real	size  = 0.25		{ prompt = "SIZE value (degrees)"	}

bool    samp     = no           { prompt = "Broadcast result table(s)?" }
bool    plot     = no           { prompt = "Plot result table(s)?"      }
bool    display  = no           { prompt = "Display field?"             }
bool    overplot = no           { prompt = "Overplot display?"           }
string	output = "STDOUT"	{ prompt = "Output filename"		}
string	format = "ascii"		{ prompt = "Output format",
				    min="ascii|csv|votable|fits|xml|raw"} 
int	status = 0		{ prompt = "Service status code"	}

begin
	string  lname, lres, lpos, url, loname, tname, ltype, args, tcat
	bool	ldisp, lover, lplot
	real	ra, dec
	int     nread, len, nres, nfields, nrows, ncols


	# Get params to local variables.
        #lres    = resource
        #lname   = fields
        lpos    = ""
        lname   = ""
        print (resource) | translit ("STDIN"," ","+") | scan (lres)
        print (fields) | translit ("STDIN"," ","+") | scan (lname)
        if (fields == "") {
            lpos = pos
        }

	ltype  = format
	loname = output
	ldisp  = display
	lover  = overplot
	lplot  = plot

	tcat = mktemp ("tmp$tcat")
        files (lres, sort-) | count ("STDIN") | scan (nres)
        if (substr (lname, 1, 1) == "@") {
            if (imaccess (substr (lname, 2, strlen (lname))) == yes) {
                sections (lname, opt="root", > tcat)
                count (tcat) | scan (nfields)
            }
        } else {
            if (lpos == "") {
                nfields = 1
            } else {
                files (lname, sort-, > tcat)
                count (tcat) | scan (nfields)
            }
        }

        # Simple error checking.
        if (loname == "STDOUT" && ltype == "fits") {
            error (0, "FITS output not allowed to STDOUT")
        }


        if (nres > 1 || nfields > 1) {
            vodata (lres, "@"//tcat, size=size, samp=samp, count-, all-,
                type="image", output=output, format=format)
	    delete (tcat, verify-, >& "dev$null")
            return
        }

	# Resolve resource name
	if (lres == "")
	    error (0, "No resource specified")
	    
        if (substr (lres, 1, 7) == "http://") {
            url  = lres
            len = strlen (url)
            if (substr(url,len,len) != '?' && substr(url,len,len) != '&') {
                if (strstr ("?", url) == 0)
                    url = url // "?"
                else
                    url = url // "&"
            }

        #} else if (access ("uparm$url") == yes) {
        #    list = "uparm$url" ; nread = fscan (list, url) ; list = ""
        } else {
            regdb ("resolve", lres, type="I", >& "dev$null")
	    if (regdb.status == 0 && regdb.svctype != "I")  {
                error (0, "Resource '" // lres // "' is not a catalog service")
            } else if (regdb.status == 1) {
                url   = regResolver (lres)  # not found, query the Registry
            } else {
                url   = regdb.url
            }

            if (url == "INDEF") {
                error (0, "Resource '" // lres // "' is not a known service")
            }
        }

        #  Make sure the URL has a trailing '/' or '&'
        len = strlen (url)
        if (substr(url,len-4,len) == "&amp;")
            url = substr (url, 1, len-5)
        if (substr(url,len,len) != '?' && substr(url,len,len) != '&') {
            if (strstr ("?", url) == 0)
                url = url // "?"
            else
                url = url // "&"
        }


	# Determine the query params from the image WCS.
	if (imaccess (lname) == no) {
            if (lpos == "") {
                sesame (lname, verbose-)
                ra  = sesame.ra
                dec = sesame.dec

                if (ldisp)
                    dss (lname, use_disp+)
                lname = "cache$" // lname // ".fits"
                args = "POS=" // lpos // "&SIZE=" // size

            } else {
                lname = ""
                args = "POS=" // lpos // "&SIZE=" // size
            }

	} else {
	    wcsinfo (lname)
	    args = "POS=" // wcsinfo.pos // "&SIZE=" // wcsinfo.size
	}

	if (vo.runid != "")
	    args = args // "&RUNID=" // vo.runid

	# Create a temporary output name.
	tname = mktemp ("tmp$raw") // ".xml"

	# Call the raw service via the URLGET generic task.
	urlget (url//args, tname, extn="", use_cache+, cache="cache$")

        if (samp)
            samp ("loadVOTable", tname, id=lres, >& "dev$null")

	# Get the size of the result table.
	votsize (osfn(tname)) |& scan (nrows, ncols)
	if (nrows == 0) {
	    print ("No results found.")
	    delete (tcat, verify-, >& "dev$null")
	    delete (tname, verify-, >& "dev$null")
	    status = 1
	    return
	}


        # Do the overlay if we displayed an image.
        if (ldisp && lover && nrows > 0) {
            votpos (tname, out=tcat, verb-, number+, header-)
            taboverlay (lname, tcat, lab=1, ra=2, dec=3, mkcolor=207)
        }
        if (lplot)
            votpos (tname, out="", verb-, plot+, number-, overplot=lover)

	# If we didn't want a VOTable, convert the file and rename to
	# desired output name
	if (loname != "") 
            votcopy (tname, loname, ltype, header+, verb-)

	delete (tcat, verify-, >& "dev$null")
	delete (tname, verify-, >& "dev$null")
end


