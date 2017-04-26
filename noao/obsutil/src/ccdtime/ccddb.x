# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# General text database routines.

# Symbol table definitions.
define	LEN_INDEX	10		# Length of symtab index
define	LEN_STAB	512		# Length of symtab
define	SZ_SBUF		512		# Size of symtab string buffer
define	SYMLEN		40		# Length of symbol structure
define	SZ_DBVAL	79		# Size of database value string

# Symbol table structure
define	DBVAL		Memc[P2C($1)]	# Database value string


# DBOPEN -- Open database and store the requested information in symbol table.

pointer procedure dbopen (dname, fname, kname, ename)

char	dname[ARB]			#I Directory name
char	fname[ARB]			#I File name
char	kname[ARB]			#I Key name
char	ename[ARB]			#I Entry name
pointer	db				#O Database symbol table pointer

int	fd, found, open(), fscan(), nscan()
pointer	sp, pname, name, key, str, sym
pointer	stopen(), stenter()
bool	streq(), strne()
errchk	open, stopen, stenter, fscan, dberror

begin
	call smark (sp)
	call salloc (pname, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open database.
	call sprintf (Memc[pname], SZ_FNAME, "%s%s")
	    call pargstr (dname)
	    call pargstr (fname)
	fd = open (Memc[pname], READ_ONLY, TEXT_FILE)

	# Strip entry name whitespace and convert to lower case.
	call strcpy (ename, Memc[name], SZ_LINE)
	call xt_stripwhite (Memc[name])
	call strlwr (Memc[name])

	# List entries in database.
	if (Memc[name] == '?') {
	    Call printf ("Entries for %s in database %s:\n")
		call pargstr (kname)
		call pargstr (Memc[pname])
	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[key], SZ_FNAME)
		call gargwrd (Memc[str], SZ_LINE)
		call gargwrd (Memc[str], SZ_LINE)
		if (nscan()<3 || Memc[key]=='#' || strne (Memc[key], kname))
		    next
		call printf ("\t%s\n")
		    call pargstr (Memc[str])
	    }
	    call close (fd)
	    call sfree (sp)
	    return (NULL)
	}
	    
	# Find entry.
	found = 0
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[key], SZ_FNAME)
	    call gargwrd (Memc[str], SZ_LINE)
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan()<3 || Memc[key]=='#' || strne (Memc[key], kname))
		next
	    found = 1
	    call strlwr (Memc[str])
	    if (streq (Memc[str], Memc[name])) {
		found = 2
		break
	    }
	}

	# Check if entry was found.
	if (found != 2) {
	    call close (fd)
	    if (found != 1)
		call dberror ("DBOPEN: Database entry not found", kname)
	    else
		call dberror ("DBOPEN: Database entry not found", ename)
	}

	# Create symbol table.
	db = stopen (ename, LEN_INDEX, LEN_STAB, SZ_SBUF)

	# Read the file and enter the parameters in the symbol table.
	sym = stenter (db, Memc[key], SYMLEN)
	call strcpy (ename, DBVAL(sym), SZ_DBVAL)
	while (fscan(fd) != EOF) {
	    call gargwrd (Memc[key], SZ_FNAME)
	    call gargwrd (Memc[str], SZ_LINE)
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan()>0 && (streq(Memc[key],"end") || streq(Memc[key],kname)))
		break
	    if (nscan() < 3 || Memc[key] == '#')
		next
	    sym = stenter (db, Memc[key], SYMLEN)
	    call strcpy (Memc[str], DBVAL(sym), SZ_DBVAL)
	}

	call close (fd)
	call sfree (sp)

	return (db)
end


# DBCLOSE -- Close the database symbol table pointer.

procedure dbclose (db)

pointer	db			# Database symbol table pointer

begin
	if (db != NULL)
	    call stclose (db)
end


# DBGETD -- Get double database parameter.

double procedure dbgetd (db, param, arg1, arg2)

pointer	db			# Database symbol table pointer
char	param[ARB]		# Database parameter
char	arg1[ARB], arg2[ARB]	# Optional arguments

char	str[SZ_LINE]
int	ip, ctod()
double	dval
errchk	dbgstr

begin
	call dbgstr (db, param, arg1, arg2, str, SZ_LINE)

	ip = 1
	if (ctod (str, ip, dval) <= 0)
	    call dberror ("DBGETD: Database parameter not double", param)
	return (dval)
end


# DBGSTR -- Get string valued parameter.

procedure dbgstr (db, param, arg1, arg2, str, maxchar)

pointer	db			# Database symbol table pointer
char	param[ARB]		# Database parameter
char	arg1[ARB], arg2[ARB]	# Optional arguments
char	str[maxchar]		# Database parameter value
int	maxchar			# Maximum characters for string

pointer	sp, param1, sym, stfind()
errchk	dberror

begin
	call smark (sp)
	call salloc (param1, SZ_LINE, TY_CHAR)

	sym = NULL
	if (arg1[1] != EOS && arg2[1] != EOS) {
	    call sprintf (Memc[param1], SZ_LINE, "%s(%s,%s)")
		call pargstr (param)
		call pargstr (arg1)
		call pargstr (arg2)
	    sym = stfind (db, Memc[param1])
	    if (sym == NULL) {
		call sprintf (Memc[param1], SZ_LINE, "%s(%s,%s)")
		    call pargstr (param)
		    call pargstr (arg2)
		    call pargstr (arg1)
		sym = stfind (db, Memc[param1])
	    }
	}
	if (sym == NULL && arg1[1] != EOS) {
	    call sprintf (Memc[param1], SZ_LINE, "%s(%s)")
		call pargstr (param)
		call pargstr (arg1)
	    sym = stfind (db, Memc[param1])
	}
	if (sym == NULL && arg2[1] != EOS) {
	    call sprintf (Memc[param1], SZ_LINE, "%s(%s)")
		call pargstr (param)
		call pargstr (arg2)
	    sym = stfind (db, Memc[param1])
	}
	if (sym == NULL)
	    sym = stfind (db, param)

	call sfree (sp)

	if (sym == NULL)
	    call dberror ("DBGSTR: Database parameter not found", param)
	call strcpy (DBVAL(sym), str, maxchar)
end


# DBERROR -- Print database error.

procedure dberror (errstr, param)

char	errstr[ARB]		# Error string
char	param[ARB]		# Parameter
char	errmsg[SZ_LINE]		# Error message

begin
	call sprintf (errmsg, SZ_LINE, "%s (%s)")
	    call pargstr (errstr)
	    call pargstr (param)
	call error (1, errmsg)
end
