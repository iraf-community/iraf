#{  RAWCALLER -- Call a service URL and return the raw result.  This is
#   most often needed in the case of a TABULARSKYSERVICE where the 
#   ServiceURL responds with the entire table.

procedure rawcaller (svc_url)

string	svc_url			{ prompt = "Service URL"		}

string	output = "STDOUT"	{ prompt = "Output filename"		}
string	otype = "raw"		{ prompt = "Output format",
				    min="ascii|csv|votable|fits|raw"	} 
int	status = 0		{ prompt = "Service status code"	}

begin
	string  url, oname, tname, ctype

	url   = svc_url
	ctype = otype
	oname = output


	# Create a temporary output name.
	tname = mktemp ("tmp$raw") // ".xml"

	# Call the raw service via the DALCLIENT generic task.
	dalclient (url, "raw", otype="votable", output=tname)

	# If we didn't want a VOTable, convert the file and rename to
	# desired output name
	if (oname == "STDOUT") {
	    if (ctype == "votable" || ctype == "raw") {
	        printf ("cat %s\n", osfn(tname)) | cl()
	    } else {
		oname = mktemp ("tmp$raw")
	        stilts ("tcopy", "ifmt=votable", "ofmt="//ctype, 
		    osfn(tname), osfn(oname)) |& \
		match("WARNING", "STDIN", stop+)

	        # Use a shell escape to the system 'cat' task to output
		# the results.  This avoids terminal width limits in using
		# something like TYPE and by piping it to the CL for execution
		# make the output part of our context so we can be piped to
		# another task.

	        printf ("cat %s\n", osfn(oname)) | cl()

	        delete (oname, verify-)
	    }
	} else {
	    if (ctype == "votable") {
		copy (tname, oname)
	    } else {
	        stilts ("tcopy", "ifmt=votable", "ofmt="//ctype, 
		    osfn(tname), osfn(oname)) |& \
		match("WARNING", "STDIN", stop+)
	    }
	}

	delete (tname, verify-)
end
