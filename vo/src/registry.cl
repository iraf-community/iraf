#{  REGISTRY -- Do a Registry query.

procedure registry (query)

string	query			{ prompt = "Query terms"		      }

string	type  	  = ""		{ prompt = "Sevice type constraint"	      }
string	bandpass  = ""		{ prompt = "Bandpass constraint"	      }
string	content   = ""		{ prompt = "ContentLevel constraint"	      }
string	sql       = ""		{ prompt = "SQL Query terms"		      }

string	fields="Title,Subject,CoverageSpectral"  \
				{ prompt = "Verbose output fields"	      }
bool	header    = yes		{ prompt = "Header output?"		      }
bool	verbose   = no		{ prompt = "Verbose output?"		      }
bool	interactive = no	{ prompt = "Interactive processing?"	      }

int	record    = -1		{ prompt = "Record number to list?"	      }
bool	metadata  = no		{ prompt = "List resource metadata?"	      }
int	nlines    = 5		{ prompt = "Number lines in Vizier preview"   }
 
int	qres      = 0		{ prompt = "Saved resource record pointer"    }
int	qcount    = 0		{ prompt = "Saved record count"	              }
string	qstring   = ""		{ prompt = "Saved query string"	              }
string	qbandpass = ""		{ prompt = "Saved bandpass constraint"        }
string	qcontent  = ""		{ prompt = "Saved ContentLevel constraint"    }
string	qsvc      = ""		{ prompt = "Saved service type constraint"    }
string	qsql      = ""		{ prompt = "Saved SQL constraint"             }

int	status    = 0		{ prompt = "Service status code"	      }

begin
    string qterm, fstr, ffile, qstr, id, typ, str
    string bpass, clevel, sqlstr, stype, ch
    bool   verb, meta, interact, summary
    int    res, count, istart, iend, n, vnlines, rec


    verb     = verbose
    fstr     = fields
    meta     = metadata
    stype    = type
    bpass    = bandpass
    clevel   = content
    sqlstr   = sql
    interact = interactive
    vnlines  = nlines



    # Set the environment.
    reset clobber = yes
    summary  = no

    # Print the resource alias list
    if (query == "aliases") {
	if (access ("home$registry.dat") == yes)
	    type ("home$registry.dat")
	return
    } else if (query == "alias") {
	return
    }

    # See if all we want is a list of the available metadata.
    if (meta) {
	regmetalist(all-)
	return
    }

submit:
    # Build a sql predicate string from the contraints (if any).
    qstr = ""
    if (stype != "") {
	str = strlwr (stype)
	if (str == "sia" || substr (str,1,2) == "im")
            qstr = "(Tag like '%images%')"
	else
            qstr = "(xml like '%" // stype // "%')"
    }
    if (bpass != "") {
        str = "([coverage/waveband] like '%" // bpass // "%')"
	if (qstr != "")
	    qstr = qstr // " AND " // str
	else
	    qstr = str
    }
    if (clevel != "") {
        str = "([content/contentLevel] like '%" // clevel // "%')"
	if (qstr != "")
	    qstr = qstr // " AND " // str
	else
	    qstr = str
    }
    if (sqlstr != "") {
        str = "(" // sqlstr // ")"
	if (qstr != "")
	    qstr = qstr // " AND " // str
	else
	    qstr = str
    }

    # Get a temp file so we can loop over the fields.  Dump the fields
    # string to the file for easy reading.
    ffile = mktemp ("tmp$reg")
again:
    if (record > 0 || fstr == "all") {
	regmetalist (all+, > ffile)
    } else {
        print (fstr) | translit ("STDIN",",","\n",del-, > ffile)
    }
    

    # Do the query
    if (record >= 0) {
        res   = qres
        count = qcount
	if (record == 0) {
	    istart = 0
	    iend   = qcount 
	} else {
	    istart = record - 1
	    iend   = record 
	    verb   = yes	# reset verbose flag for single full record
	}
	qterm  = qstring
	stype  = qsvc
	bpass  = qbandpass
	clevel = qcontent
	sqlstr = qsql
        if (header)
            printf ("#\n#  Record: %d\n#\n", record)

    } else {
        qterm  = query
	if (qstr != "") 
            res    = regSearch (qstr, qterm, 0)
	else
            res    = regSearch (qterm, 0)
        count  = regResCount (res)
	istart = 0
	iend   = count
        if (header)
	    printf ("#\n#  Found %d records\n#\n", count)
    }

    # Dump the query constraints we used in the search.
    if (header) {
        printf ("#  Registry Query Constraints:\n")
        if (qterm != "")  printf ("#\t  Query Term: '%s'\n", qterm)
        if (stype != "")  printf ("#\t ServiceType:  %s\n", stype)
        if (bpass != "")  printf ("#\t    Bandpass:  %s\n", bpass)
        if (clevel != "") printf ("#\tContentLevel:  %s\n", clevel)
        if (sqlstr != "") printf ("#\tSQL: %s\n", sqlstr)
        printf ("#\n")
    }

    if (record <= 0 && header) {
        printf ("#No  ShortName          ServiceType  Identifier\n")
        printf ("#--  ---------          -----------  ----------\n#\n")
    }

    for (i=istart; i < iend; i=i+1) {
	if (record <= 0 && fstr != "all") {
            printf ("%3d  %-18.18s %-10.10s   %s\n",
                  (i+1), 
                  trim (regValue (res, "ShortName", i)),
                  trim (regValue (res, "ServiceType", i)),
                  trim (regValue (res, "Identifier", i)))
	} else if (fstr == "all")
            printf ("Record:  %d\n", i)

	if (!summary && verb) {
	    list = ffile
	    while (fscan (list, str) != EOF) {
		printf ("   %12.12s: ", str)
		line = regValue (res, str, i)
		prettystr (trim(line))
	    }
	    list = ""
            printf ("\n")
	}
    }

    # Save the resource pointer and record count in case we come back.
    if (record < 0) {
        qres      = res
        qcount    = count
        qstring   = qterm
        qbandpass = bpass
        qcontent  = clevel
        qsvc      = stype
        qsql      = sqlstr
    }


    if (interact) {
nextcmd:
	printf ("\nCommand? ")
	summary = no

	ch = cl.ukey
	if (substr(ch,1,4) == "\\015") {
	    goto nextcmd
	}

	switch (ch) {
	case "?":			# print help
	    {	print ("\n\n    REGISTRY Command Summary:")
		print ("	t    constrain by service Type")
		print ("	b    constrain by Bandpass")
		print ("	r    view details of individual Record")
		print ("	s    print Summary table of results")
		print ("	p    Preview a vizier table")
		print ("	v    set Verbose flag")
		print ("	q    Quit")
	        goto nextcmd
	    }
	case "t":			# Record
	    {   printf ("\nService Type? ")
	        n = scan (stype)
		record = -1
		summary = yes
	        goto submit
	    }
	case "b":			# Record
	    {   printf ("\nBandpass? ")
	        n = scan (bpass)
		record = -1
		summary = yes
	        goto submit
	    }
	case "r":			# Record
	    {   printf ("\nRecord number? ")
	        n = scan (record)
	        goto again
	    }
	case "s": 			# Summary list
	    {   record = 0
		summary = yes
	        goto again
	    }
	case "p": 			# Preview a VizieR table
	    {   printf ("\nRecord number? ")
	        n = scan (rec)
                id = trim (regValue (res, "Identifier", rec-1))
		typ = strlwr (trim (regValue (res, "ServiceType", rec-1)))
		if (substr(typ,1,7) != "tabular") {
		    printf ("\nError: Record %d is not a VizieR table\n", n)
		    goto nextcmd
		} else {
		    vizier (id, output="STDOUT", otype="ascii", all-) | \
			head ("STDIN", nlines=vnlines)
		}
	        goto nextcmd
	    }
	case "v": 			# Set verbose
	    {   verb = yes
	        goto again
	    }
	case "q":			# Quit
	    {   record = -1
	        print ("")
	    }
	}
    }

    # Clean up.
    delete (ffile, verify-, >& "dev$null")
end
