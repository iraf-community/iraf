#
#  RESDB -- Utility routines to manage the local resource database


bool procedure rdb_lookup (resdb, term, type, sname, ivorn, svcurl)

char	resdb[ARB]				#i resource database
char	term[ARB]				#i search term
char	type[ARB]				#i service type
char	sname[ARB]				#i short name
char	ivorn[ARB]				#i ivorn string
char	svcurl[ARB]				#i URL string

char	typ[SZ_FNAME], alias[SZ_FNAME], bpass[SZ_FNAME], line[SZ_LINE]
char	ivo[SZ_FNAME], shortname[SZ_FNAME], url[SZ_FNAME]
int	fd, ip, i, lnum

int	access(), open(), getline()
bool	streq()

begin
	if (access (resdb, 0, 0) == NO) {
	    call eprintf ("Error: cannot open resdb '%s'\n")
		call pargstr (resdb)
	    return
	}

	call strlwr (term)

	# Open the resource database
	fd = open (resdb, READ_ONLY, TEXT_FILE)
	for (lnum=0; getline (fd, line) != EOF; lnum=lnum+1) {

	    if (line[1] == '#' || line[1] == EOS || line[1] == '\n')
		next

	    call aclrc (type,      SZ_FNAME)
	    call aclrc (alias,     SZ_FNAME)
	    call aclrc (bpass,     SZ_FNAME)
	    call aclrc (ivo,       SZ_FNAME)
	    call aclrc (url,       SZ_FNAME)
	    call aclrc (shortname, SZ_FNAME)

	    ip = 1
	    for (i=1; line[ip] != ','; i=i+1) { 	# type string
		typ[i] = line[ip]
		ip = ip + 1
	    }
	    call strlwr (typ)
	    ip = ip + 1

	    for (i=1; line[ip] != ','; i=i+1) { 	# alias string
		alias[i] = line[ip]
		ip = ip + 1
	    }
	    call strlwr (alias)
	    ip = ip + 1

	    for (i=1; line[ip] != ','; i=i+1) { 	# bandpass string
		bpass[i] = line[ip]
		ip = ip + 1
	    }
	    call strlwr (typ)
	    ip = ip + 1

	    for (i=1; line[ip] != ','; i=i+1) { 	# ivorn string
		ivo[i] = line[ip]
		ip = ip + 1
	    }
	    call strlwr (typ)
	    ip = ip + 1

	    for (i=1; line[ip] != ','; i=i+1) { 	# ShortName string
		shortname[i] = line[ip]
		ip = ip + 1
	    }
	    call strlwr (typ)
	    ip = ip + 1

	    for (i=1; line[ip] != ','; i=i+1) { 	# URL string
		url[i] = line[ip]
		ip = ip + 1
	    }
	    call strlwr (typ)
	    ip = ip + 1


	    if (type[1] == EOS || typ[1] == type[1]) {
		if (streq (alias, term)) {
		    call strcpy (shortname, sname, SZ_FNAME)		
		    call strcpy (ivo, ivorn, SZ_FNAME)		
		    call strcpy (url, svcurl, SZ_FNAME)		
		    return (true)
		}
	    }
	}
	call close (fd)

	return (false)
end
