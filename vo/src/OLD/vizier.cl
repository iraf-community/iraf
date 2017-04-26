#{  VIZIER -- Call a service Vizier TABULARSKY service.

procedure vizier (tab_id)

string	tab_id			{ prompt = "Table reference"		}

string	output  = "STDOUT"	{ prompt = "Output filename"		}
string	otype   = "ascii"	{ prompt = "Output format",
				    min="ascii|csv|votable|fits"	} 
bool	all     = yes		{ prompt = "Get all matching tables?"   }
bool	verbose = no		{ prompt = "Verbose output?"   }

int	status  = 0		{ prompt = "Service status code"	}

begin
	string  id, url, oname, tname, ctype, oroot
	string  sfile, fields, catid, svc_type
	bool	verb, do_all
	int	fnum, nrecords


	id     = tab_id
	ctype  = otype
	oroot  = output
	do_all = all
	verb   = verbose

	svc_type = "tabularskyservice"
	fields   = "ServiceURL,Identifier"
	fnum     = 1


	# Create a temporary url file and output name.
	sfile = mktemp ("tmp$vizier")
	tname = mktemp ("tmp$vizier") // ".xml"

	if (do_all)
	    print (regResolver (id, svc_type, fields, -1), > sfile)
	else
	    print (regResolver (id, svc_type, fields), > sfile)

	nrecords = nresolved()

	list = sfile 
	while (fscan (list, url, catid) != EOF) {

	    if (nrecords > 1)
	        oname = oroot // "." // fnum
	    else
	        oname = oroot

	    if (verb)
		printf ("Downloading %s ... to %s\n", catid, oname)

	    # Call the Vizier service url
	    rawcaller (url, otype="votable", output=tname)

	    # Output the table.
	    tabout (tname, oname, ctype)

	    delete (tname, verify-)
	    fnum = fnum + 1
	}

end
