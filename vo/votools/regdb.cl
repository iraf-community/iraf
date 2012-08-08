#{ REGDB -- Registry DB utility.

procedure regdb (arg1, arg2, arg3)

string	arg1				{ prompt = "Command/Search Term"     }
string	arg2				{ prompt = "Search Term"	     }
string	arg3				{ prompt = "Optional arg"	     }

string	type     = ""			{ prompt = "Service type constraint" }
string	bandpass = ""			{ prompt = "Bandpass constraint"     }

string	alias    = ""			{ prompt = "Resource alias"	     }
string	ivorn    = ""			{ prompt = "Ivorn string"	     }
string	sname    = ""			{ prompt = "Short Name"	      	     }

string	svctype  = ""			{ prompt = "Resource type"	     }
string	bpass    = ""			{ prompt = "Bandpass"	      	     }
string	url      = ""			{ prompt = "Service URL"	     }
string	desc     = ""			{ prompt = "Description"	     }

bool	verbose  = no			{ prompt = "Print result?"	     }
int	status   = 0			{ prompt = "status code"	     }

begin
    string  tf, t, a, b, i, s, u, d, lterm, cmds = ""
    string  lcmd, lalias, ltype, lbpass, livorn, lsname, ldesc, lresdb, lnew
    bool    verb, found


    # Initialize the command dictionary.
    cmds   = cmds // "|list|resolve|search"
    cmds   = cmds // "|type|alias|bpass|ivorn|sname|url|desc"
    cmds   = cmds // "|add|del|update|rename|"


    if ($nargs == 1 && strdic (arg1, cmds) > 0) {
	# cl> regdb list
	lcmd = arg1
	lterm = ""

    } else if ($nargs == 3 && arg1 == "rename") {
	# cl> regdb rename <old> <new>
	lcmd = arg1
	lterm = ""
	lnew = arg3

    } else if ($nargs == 1 || strdic (arg1, cmds) == 0) {
	# cl> regdb <ivorn> | <alias> | <sname>
	lcmd = "resolve"
	lterm = strlwr (arg1)

    } else if ($nargs > 1 && strdic(arg1,cmds) > 0 && strdic(arg2,cmds) == 0) {
	# cl> regdb list  <ivorn> | <alias> | <sname>
	lcmd = arg1
	lterm = strlwr (arg2)
    } else {
	lcmd = arg1
	lterm = ""
    }

    lalias = strlwr (alias)
    ltype  = strlwr (substr (type,1,1))
    lbpass = strlwr (bandpass)

    lsname = strlwr (sname)
    livorn = strlwr (ivorn)
    lresdb = vo.resdb			# from package param file
    ldesc  = ""
    verb   = verbose

    # Dump the database to a parsable file.
    tf = mktemp ("tmp$tf")
    translit (lresdb, ",", " ") | fields ("STDIN", "1-", > tf)

	
    status = 0

    # Resolve the search term.
    if (lcmd != "list" && lterm != "") {
	found = no
	delete ("uparm$url", verify-, >& "dev$null")
        list = tf
	status = 1
        while (fscan (list, t, a, b, i, s, u, d) != EOF) {

	    if (lterm == strlwr(a) || lterm == strlwr(b) || 
		lterm == strlwr(i) || lterm == strlwr(s)) {

		    if ((lbpass != "" && lbpass != strlwr(b)))
		        next
		    if ((ltype != "" && ltype != strlwr(t)))
		        next

	    	    printf ("%s\n", d) | \
			translit ("STDIN", ":", " ", collapse+) | scan (ldesc)
	            svctype  = t ; alias = a ; bpass = b ; ivorn = i
	            sname = s ; url   = u ; desc  = ldesc
		    print (u, > "uparm$url")
		    found = yes
		    status = 0
		    break;
	    }
        }
        list = ""

	if (found == no) {
	    printf ("Resource '%s' not found\n", lterm)
	    #
	    # FIXME -- Should we call the Registry here for a search????
	    #
	    svctype  = "" ; bpass = "" ; ivorn = "" ; desc  = "" ; url   = ""
	    if (lalias != "") alias = lterm
	    if (lsname != "") sname = lterm
	    status = 1
	    goto done_
	}
	;
    }


    if (lcmd == "list") {				# LIST
        list = tf
	while (fscan (list, t, a, b, i, s, u, d) != EOF) {

	    if (lterm == "") {
		# cl> regdb list bandpass=optical
	        if (ltype != "" && ltype != strlwr(t))
		     next
	        if (lbpass != "" && strstr (lbpass, strlwr(b)) == 0)
		     next

	        printf ("%-10s  ", a)
	        printf ("%s  ", t)
	        if (lbpass == "") printf ("%-8s  ", b)
	        printf ("%s\n", d) | translit ("STDIN", ":", " ", collapse+)

	    } else {
		# cl> regdb list  <ivorn> | <alias> | <sname>

	        if (lterm == strlwr(a) || lterm == strlwr(b) || 
		    lterm == strlwr(i) || lterm == strlwr(s)) {
	        	printf ("     Alias: %s\n", a)
	        	printf (" ShortName: %s\n", s)
	        	printf ("     Title: %s\n", d)
	        	printf ("      Type: %s\n", t)
	        	printf ("  Bandpass: %s\n", b)
	        	printf ("     Ivorn: %s\n", i)
	        	printf ("    SvcURL: %s\n", u)
		        #break;
		}
	    }
	}
        list = ""

    } else if (lcmd == "type") {			# TYPE
	print (t)

    } else if (lcmd == "alias") {			# ALIAS
	if (lterm != "") {
	    print (a)
	} else {
            list = tf
	    while (fscan (list, t, a, b, i, s, u, d) != EOF) {
	        if ((lalias!="" && lalias == strlwr(a)) ||
		    (ldesc !="" && ldesc == strlwr(d))  ||
		    (livorn!="" && livorn == strlwr(i)) ||
	            (ltype != "" && ltype == strlwr(t))   ||
	            (lbpass != "" && lbpass == strlwr(b)) ||
		    (lsname!="" && lsname == strlwr(s)) ) {

			if (verbose)
	        	    print (a)
			else
			    alias = a
		}
	    }
            list = ""
	}
	svctype  = "" ; bpass = "" ; ivorn = "" ; desc  = "" ; url   = ""
	alias    = "" ; sname = ""
    } else if (lcmd == "bpass") {			# BPASS
	if (lterm != "") {
	    print (b)
	} else {
            list = tf
	    while (fscan (list, t, a, b, i, s, u, d) != EOF) {
	        if ((lalias!="" && lalias == strlwr(a)) ||
		    (ldesc !="" && ldesc == strlwr(d))  ||
		    (livorn!="" && livorn == strlwr(i)) ||
	            (ltype != "" && ltype == strlwr(t))   ||
	            (lbpass != "" && lbpass == strlwr(b)) ||
		    (lsname!="" && lsname == strlwr(s)) ) {

			if (verbose)
	        	    print (b)
			else
			    bpass = b
		}
	    }
            list = ""
	}
	svctype  = "" ; bpass = "" ; ivorn = "" ; desc  = "" ; url   = ""
	alias    = "" ; sname = ""
    } else if (lcmd == "ivorn") {			# IVORN
	if (lterm != "") {
	    print (i)
	} else {
            list = tf
	    while (fscan (list, t, a, b, i, s, u, d) != EOF) {
	        if ((lalias!="" && lalias == strlwr(a)) ||
		    (ldesc !="" && ldesc == strlwr(d))  ||
		    (livorn!="" && livorn == strlwr(i)) ||
	            (ltype != "" && ltype == strlwr(t))   ||
	            (lbpass != "" && lbpass == strlwr(b)) ||
		    (lsname!="" && lsname == strlwr(s)) ) {

			if (verbose)
	        	    print (i)
			else
			    ivorn = i
		}
	    }
            list = ""
	}
	svctype  = "" ; bpass = "" ; ivorn = "" ; desc  = "" ; url   = ""
	alias    = "" ; sname = ""
    } else if (lcmd == "sname") {			# SNAME
	if (lterm != "") {
	    print (s)
	} else {
            list = tf
	    while (fscan (list, t, a, b, i, s, u, d) != EOF) {
	        if ((lalias!="" && lalias == strlwr(a)) ||
		    (ldesc !="" && ldesc == strlwr(d))  ||
		    (livorn!="" && livorn == strlwr(i)) ||
	            (ltype != "" && ltype == strlwr(t))   ||
	            (lbpass != "" && lbpass == strlwr(b)) ||
		    (lsname!="" && lsname == strlwr(s)) ) {

			if (verbose)
	        	    print (s)
			else
			    sname = s
		}
	    }
            list = ""
	}
	svctype  = "" ; bpass = "" ; ivorn = "" ; desc  = "" ; url   = ""
	alias    = "" ; sname = ""
    } else if (lcmd == "url") {				# URL
	if (lterm != "") {
	    print (u)
	} else {
            list = tf
	    while (fscan (list, t, a, b, i, s, u, d) != EOF) {
	        if ((lalias!="" && lalias == strlwr(a)) ||
		    (ldesc !="" && ldesc == strlwr(d))  ||
		    (livorn!="" && livorn == strlwr(i)) ||
	            (ltype != "" && ltype == strlwr(t))   ||
	            (lbpass != "" && lbpass == strlwr(b)) ||
		    (lsname!="" && lsname == strlwr(s)) ) {

			if (verbose)
	        	    print (u)
			else
			    url = u
		}
	    }
            list = ""
	}
	svctype  = "" ; bpass = "" ; ivorn = "" ; desc  = "" ; url   = ""
	alias    = "" ; sname = ""
    } else if (lcmd == "desc") {			# DESCRIPTION
	if (lterm != "") {
	    print (d)
	} else {
            list = tf
	    while (fscan (list, t, a, b, i, s, u, d) != EOF) {
	        if ((lalias!="" && lalias == strlwr(a)) ||
		    (ldesc !="" && ldesc == strlwr(d))  ||
		    (livorn!="" && livorn == strlwr(i)) ||
	            (ltype != "" && ltype == strlwr(t))   ||
	            (lbpass != "" && lbpass == strlwr(b)) ||
		    (lsname!="" && lsname == strlwr(s)) ) {

			if (verbose)
	        	    print (d)
		}
	    }
            list = ""
	}
	svctype  = "" ; bpass = "" ; ivorn = "" ; desc  = "" ; url   = ""
	alias    = "" ; sname = ""


    } else if (lcmd == "add") {				# ADD TO DB

    } else if (lcmd == "del") {				# DELETE FROM DN

    } else if (lcmd == "update") {			# UPDATE DB

    } else if (lcmd == "rename") {			# RENAME ALIAS
	# Need arg3


    } else if (lcmd == "resolve" || lcmd == "search") {	# RESOLVE
        list = tf
 	status = 1
	while (fscan (list, t, a, b, i, s, u, d) != EOF) {
	    if ((lalias != "" && lalias == strlwr(a)) ||
		(lterm != "" && lterm == strlwr(a))   ||
		(ldesc != "" && ldesc == strlwr(d))   ||
		(livorn != "" && livorn == strlwr(i)) ||
		(lterm == "") ||
		(lsname!="" && lsname == strlwr(s)) ) {

	            if ((ltype != "" && ltype != strlwr(t)) ||
	                (lbpass != "" && lbpass != strlwr(b)) ) 
		    	    next

	    	    printf ("%s\n", d) | \
			translit ("STDIN", ":", " ", collapse+) | scan (ldesc)
	            svctype  = t ; alias = a ; bpass = b ; ivorn = i
	            sname = s ; url   = u ; desc  = ldesc
		    status = 0
		    break
	    }
	}
        list = ""

    } else {
	printf ("Command '%s' not found\n", lcmd)
	status = 1
    }

done_:
    type = ""
    bandpass = ""

    del (tf, verify-)
end
