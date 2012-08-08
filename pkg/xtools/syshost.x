include	<clset.h>
include	<ctotok.h>


# SYSHOST -- If a task which calls this routine is executed from the host
# command line (i.e. not through a CL) set any parameters not set on the
# command line (e.g. with keyword=value or @file arguments).  The application
# provides three files to search in order.  The first two are keyword=value
# files and the last is a parameter file.  The parameter file may be encoded
# as a compiled procedure (see txtcompile).  For this reason xt_txtopen is
# used which transparently handles disk text files and text encoding
# procedures.
#
# The process type is the return value.
# The show_pset argument is the name of a boolean parameter to query
# for showing the default parameter set.  The value of this is returned
# for show_val.  If the parameter is not used (a null string) or was
# not specified by the user the return value is false.

int procedure syshost (keyfile1, keyfile2, parfile, show_pset, show_val)

char	keyfile1[ARB]			#I Keyword file
char	keyfile2[ARB]			#I Keyword file
char	parfile[ARB]			#I Parameter file
char	show_pset[ARB]			#I Parameter for requestiong show
bool	show_val			#O Result of show request
int	stat				#R Return value

char	line[SZ_LINE], param[SZ_FNAME], value[SZ_LINE]
int	i, ip, fd, tok

bool	clgetb(), streq()
int	clstati(), access(), fscan(), ctotok(), strncmp(), xt_txtopen()
pointer	clc_find()
errchk	xt_getpars, xt_txtopen

begin
	# Check if the task is called from the host.
	stat = clstati (CL_PRTYPE)
	if (stat != PR_HOST)
	    return (stat)

	# Read user keyword=value files.
	if (keyfile1[1] != EOS && access(keyfile1,0,0) == YES)
	    call xt_getpars (keyfile1)
	if (keyfile2[1] != EOS && access(keyfile2,0,0) == YES)
	    call xt_getpars (keyfile2)

	# Read parameter file.
	if (parfile[1] != EOS &&
	    (access(parfile,0,0)==YES || strncmp (parfile, "proc:", 5)==0)) {

	    # Open parameter file.
	    fd = NULL
	    fd = xt_txtopen (parfile)

	    # Check for request to show default parameters.
	    if (show_pset[1] != EOS && clc_find(show_pset,param,SZ_FNAME)>0)
	        show_val = clgetb (show_pset)
	    else
	        show_val = false
	    
	    # Scan parameter file lines and parse them.
	    while (fscan (fd) != EOF) {
		call gargstr (line, SZ_LINE)

		ip = 1
		if (ctotok (line, ip, param, SZ_FNAME) != TOK_IDENTIFIER)
		    next
		if (streq (param, "mode"))
		    next
		for (i=0; i<3 && ctotok(line,ip,value,SZ_LINE)!=TOK_EOS;) {
		    if (value[1] == ',')
			i = i + 1
		}
		tok = ctotok (line, ip, value, SZ_LINE)
		switch (tok) {
		case TOK_NUMBER, TOK_STRING, TOK_IDENTIFIER:
		    ;
		default:
		    value[1] = EOS
		}

		# Enter in clcache if not already defined.
		if (clc_find (param, line, SZ_LINE) == NULL)
		    call clc_enter (param, value)

		# Show parameter if desired.
		if (show_val) {
		    switch (tok) {
		    case TOK_STRING:
			call printf ("%s = ""%s""\n")
			    call pargstr (param)
			    call pargstr (value)
		    default:
			call printf ("%s = %s\n")
			    call pargstr (param)
			    call pargstr (value)
		    }
		}
	    }

	    # Close parameter file.
	    call xt_txtclose (fd)
	}

	return (stat)
end


# The following are copies of sys_getpars and sys_paramset with the
# following changes.
#   - Return an error rather than a warning for a bad syntax
#   - Enter parameter in CL cache only if not previously set


include	<ctype.h>

define	SZ_VALSTR	1024
define	SZ_CMDBUF	(SZ_COMMAND+1024)

# XT_GETPARS -- Read a sequence of param=value parameter assignments from
# the named file and enter them into the CLIO cache for the task.

procedure xt_getpars (fname)

char	fname			# pset file

bool	skip
int	lineno, fd
pointer	sp, lbuf, err, ip
int	open(), getlline()
errchk	open, getlline

begin
	call smark (sp)
	call salloc (lbuf, SZ_CMDBUF, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	# Skip whitespace for param = value args in a par file.
	skip = true

	lineno = 0
	while (getlline (fd, Memc[lbuf], SZ_CMDBUF) != EOF) {
	    lineno = lineno + 1
	    for (ip=lbuf;  IS_WHITE (Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '#' || Memc[ip] == '\n')
		next
	    iferr (call xt_paramset (Memc, ip, skip)) {
		for (;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
		    ;
		Memc[ip] = EOS
		call salloc (err, SZ_LINE, TY_CHAR)
		call sprintf (Memc[err], SZ_LINE,
		    "Bad param assignment, line %d: `%s'\n")
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
		call close (fd)
		call error (1, Memc[err])
	    }
	}

	call close (fd)
	call sfree (sp)
end


# XT_PARAMSET -- Extract the param and value substrings from a param=value
# or switch argument and enter them into the CL parameter cache.  (see also
# clio.clcache).

procedure xt_paramset (args, ip, skip)

char	args[ARB]		# argument list
int	ip			# pointer to first char of argument
bool	skip			# skip whitespace within "param=value" args

pointer	sp, param, value, op, clc_find()
int	stridx()

begin
	call smark (sp)
	call salloc (param, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_VALSTR, TY_CHAR)

	# Extract the param field.
	op = param
	while (IS_ALNUM (args[ip]) || stridx (args[ip], "_.$") > 0) {
	    Memc[op] = args[ip]
	    op = op + 1
	    ip = ip + 1
	}
	Memc[op] = EOS

	# Advance to the switch character or assignment operator.
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	switch (args[ip]) {
	case '+':
	    # Boolean switch "yes".
	    ip = ip + 1
	    call strcpy ("yes", Memc[value], SZ_VALSTR)
	
	case '-':
	    # Boolean switch "no".
	    ip = ip + 1
	    call strcpy ("no", Memc[value], SZ_VALSTR)

	case '=':
	    # Extract the value field.  This is either a quoted string or a
	    # string delimited by any of the metacharacters listed below.

	    ip = ip + 1
	    if (skip) {
		while (IS_WHITE (args[ip]))
		    ip = ip + 1
	    }
	    call sys_gstrarg (args, ip, Memc[value], SZ_VALSTR)

	default:
	    call error (1, "IRAF Main: command syntax error")
	}

	# Enter the param=value pair into the CL parameter cache.
	if (clc_find (Memc[param], Memc[param], SZ_FNAME) == NULL)
	    call clc_enter (Memc[param], Memc[value])

	call sfree (sp)
end
