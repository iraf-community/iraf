#{  TABOUT -- Utility script to output a table in a desired format.

procedure tabout (iname, oname, otype)

string	iname 			{ prompt = "Input filename"		}
string	oname 			{ prompt = "Output filename"		}
string	otype  			{ prompt = "Output format"		}
int	status = 0		{ prompt = "Service status code"	}
int	nrows  = 0		{ prompt = "No. of output rows"		}

begin
	string  infile, outfile, type


	infile  = iname
	outfile = oname
	type    = otype


	if (access(osfn(infile)) == no)
	    return

	# Initialize the output params.
	status = 0
	nrows = -1

	# If we didn't want a VOTable, convert the file and rename to
	# desired output name
	if (outfile == "STDOUT") {
	    if (type == "votable" || type == "xml" || type == "raw") {
	        printf ("cat %s\n", osfn(infile)) | cl()
	    } else {
		oname = mktemp ("tmp$raw")
	        stilts ("tcopy", "ifmt=votable", "ofmt="//type, 
		    osfn(infile), osfn(oname)) |& \
		match("WARNING", "STDIN", stop+) |& \
		match("Can't", "STDIN", stop+)


		# Count the number of output rows.
		count ( osfn(oname) ) | scan (i)
		nrows = (i - 1)

	        # Use a shell escape to the system 'cat' task to output
		# the results.  This avoids terminal width limits in using
		# something like TYPE and by piping it to the CL for execution
		# make the output part of our context so we can be piped to
		# another task.

		if (access (oname)) {
	            printf ("cat %s\n", osfn(oname)) | cl()
	            delete (oname, verify-)
	 	}
	    }
	} else {
	    if (outfile == "plastic") {
		# Broadcast a PLASTIC message to load a table.
		plastic ("", "loadTable", osfn(infile), verbose-)

	    } else if (outfile == "topcat" || outfile == "aladin") {
		# Send a PLASTIC message to load a table.
		plastic (outfile, "loadTable", osfn(infile), verbose-)

	    } else {
	        if (type == "votable" || type == "xml" || type == "raw") {
		    copy (infile, outfile)
		} else {
	            stilts ("tcopy", "ifmt=votable", "ofmt="//type, 
		        osfn(infile), osfn(outfile)) |& \
		            match("WARNING", "STDIN", stop+) |& \
		            match("Can't", "STDIN", stop+)

		    # Count the number of output rows.
		    count ( osfn(outfile) ) | scan (i)
		    nrows = (i - 1)
		}
	    }
	}
end
