#{  GETSPEC -- Call an SSA service URL and return the raw result.

procedure getspec (resource, image)

string	resource		{ prompt = "Resource name"		}
string	image			{ prompt = "Query image"		}
string	pos   = ""		{ prompt = "POS string"			}
real	size  = 0.25		{ prompt = "SIZE value (degrees)"	}

bool    display  = no          { prompt = "Display field?"             }
bool    overlay  = no          { prompt = "Overlay display?"           }
string	output = "STDOUT"	{ prompt = "Output filename"		}
string	format = "ascii"		{ prompt = "Output format",
				    min="ascii|csv|votable|fits|raw"	} 
int	status = 0		{ prompt = "Service status code"	}

begin
	string  lname, lres, url, loname, tname, ltype, args
	bool	ldisp, lover
	real	ra, dec
	int     nread, len


	# Get params to local variables.
	lname  = image
	ltype  = format
	loname = output
	ldisp  = display
	lover  = overlay

	lres  = resource		# resolve resource name
	if (resource == "")
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
            regdb ("resolve", lres, type="S")
            url   = regdb.url
            if (regdb.svctype != "S")
                error (0, "Resource '" // lres // "' is not a spectral service")
        }


	# Determine the query params from the image WCS.
	if (imaccess (lname) == no) {
	    #error (0, "Cannot open image '" // lname // "'")
            sesame (lname, verbose-)
            ra  = sesame.ra
            dec = sesame.dec

            if (ldisp)
                dss (lname, use_disp+)
            lname = "cache$" // lname // ".fits"
            args = "POS=" // ra // "," // dec  // "&SIZE=" // size

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

	# If we didn't want a VOTable, convert the file and rename to
	# desired output name
        votcopy (tname, loname, ltype, header+, verb-)

	delete (tname, verify-)
end


