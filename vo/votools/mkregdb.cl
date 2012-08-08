#{ MKREGDB -- Make a Registry Database using constraints.

procedure mkregdb (query)

string	query				{ prompt = "Query Term"		}

string	output    = ""			{ prompt = "Output filename"	}
string	type      = ""			{ prompt = "Resource type"	}
string	bandpass  = ""			{ prompt = "Bandpass"	      	}
string	content   = ""			{ prompt = "Content Level"     	}
string	sql       = ""			{ prompt = "SQL predicate"	}

bool	header    = yes			{ prompt = "Print header?"	}
int	nresults  = 0			{ prompt = "Number of results?"	}
int	status    = 0			{ prompt = "status code"	}

begin
    string  qterm, fstr, qstr, id, typ, lout
    string  bpass, clevel, sqlstr, stype, ch
    string  t, a, b, i, u, d
    int     res, count, istart, iend, n, vnlines, rec
    bool    hdr


    reset clobber = yes 			# set the environment

    lout     = output
    hdr      = header
    stype    = type
    bpass    = bandpass
    clevel   = content
    sqlstr   = sql

    if (lout == "")
	lout = "STDOUT"

    # Build a sql predicate string from the contraints (if any).
    qstr = ""
    if (stype != "") {
	s1 = strlwr (stype)
	if (s1 == "sia" || substr (s1,1,2) == "im")
            qstr = "(Tag like '%images%')"
	else
            qstr = "(xml like '%" // stype // "%')"
    }
    if (bpass != "") {
        s1 = "([coverage/waveband] like '%" // bpass // "%')"
	if (qstr != "")
	    qstr = qstr // " AND " // s1
	else
	    qstr = s1
    }
    if (clevel != "") {
        s1 = "([content/contentLevel] like '%" // clevel // "%')"
	if (qstr != "")
	    qstr = qstr // " AND " // s1
	else
	    qstr = s1
    }
    if ($nargs > 0)
        qterm  = query
    else {
	qterm = ""
	sqlstr = "(Title like '%')"
    }
    if (sqlstr != "") {
        s1 = "(" // sqlstr // ")"
	if (qstr != "")
	    qstr = qstr // " AND " // s1
	else
	    qstr = s1
    }

    
    # Do the Registry query.
    nresults = 0
    if (qstr != "") 
      res  = regSearch (qstr, qterm, 0)
    else
      res  = regSearch (qterm, 0)
    count  = regResCount (res)
    istart = 0
    iend   = count

    if (count == 0) {
	status = 1
	return
    }

    # Dump the query constraints we used in the search.
    printf ("",							 > lout)
    if (hdr) {
	printf ("#  VO Resource utility database\n",		>> lout)
        printf ("#  \n",					>> lout)
        printf ("#  \t  Query Term: '%s'\n", qterm,		>> lout)
        printf ("#  \t ServiceType:  %s\n",  stype,		>> lout)
        printf ("#  \t    Bandpass:  %s\n",  bpass,		>> lout)
        printf ("#  \tContentLevel:  %s\n",  clevel,		>> lout)
        printf ("#  \t         SQL:  %s\n", sqlstr,		>> lout)
	printf ("#  \n",					>> lout)
	printf ("#  type,alias,bandpass,ivorn,url,title\n",	>> lout)
        printf ("#  \n",					>> lout)
    }

    for (n=istart; n < iend; n=n+1) {
                  
	t = trim (regValue (res, "ServiceType", n))
	a = trim (regValue (res, "ShortName", n))
	b = trim (regValue (res, "CoverageSpectral", n))
	i = trim (regValue (res, "Identifier", n))
	u = trim (regValue (res, "ServiceURL", n))
	d = trim (regValue (res, "Title", n))

	if (t == "" || u == "")		# probably not a data service, skip it
	    next
	if (b == "") 			# unknown bandpass matches any
	    b = "*"
	if (a == "")  			# construct ShortName from IVORN
	    a = substr (i, strldx("/",i)+1, strlen (i))

	# Fix up the values for proper entry in the database.
	if (stridx (" ", a) > 0)  a = strlwr (strsub (a, " ", "_"))
	if (stridx (",", b) > 0)  b = strlwr (strsub (b, ",", ":"))
	if (stridx (" ", d) > 0)  d = strsub (d, " ", ":")
	if (stridx (",", d) > 0)  d = strsub (d, ",", ";")

	b = strlwr (b)
	t = strlwr (t)
	if (strstr ("image", t) > 0)
	    t = "I" 
	else if (strstr ("cone", t) > 0)
	    t = "C" 
	else if (strstr ("spectra", t) > 0)
	    t = "S" 
	else 
	    t = "O" 

        printf ("%s,%s,%s,%s,%s,%s,%s\n", t, a, b, i, a, u, d, 	>> lout)
	nresults = nresults + 1
    }
end
