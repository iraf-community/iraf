# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imset.h>

.help obsdb Nov90 "Observatory Database Interface"
.ih
DESCRIPTION

These procedures provide a simple interface to a simple observatory database.
It uses environment variables if possible to allow users and sites to
set or reset the observatory information.  The observatory database is
specified by the environment parameter "obsdb" which defaults to
"noao$lib/obsdb.dat".  The observatory may be specified as "observatory"
to look first for the environment parameter "observatory" and then
for the task parameter "observatory.observatory".   The observatory
may also be specified as "obspars" to ignore the database and use the
parameters set in observatory task parameter set.  This allows setting
arbitrary values without requiring the user modify or create a database
file.  The case of the observatory identification is ignored.

PROCEDURES

.nf
	obs = obsopen (observatory)
	obs = obsvopen (observatory, verbose)
	      obsimopen (obs, im, observatory, verbose, newobs, obshead)
	      obsclose (obs)
	      obslog (obs, task, params, fd)
	val = obsget[ird] (obs, param)
	      obsgstr (obs, param, str, maxchar)
	      obsinfo (obs, fd)
.fi

DATABASE

The database file is that defined by the environment variable "obsdb".
If absent the file is "noao$lib/obsdb.dat".  The observatory name
used in the obsopen procedure is the observatory ID as defined
in the database, the special string "observatory" which uses
to the environment variable of the same name or the task parameter
"observatory.observatory", or the special string "obspars" which
uses the observatory task parameters and does not require an entry
in the database.

The database format is simply a list of keyword/value definitions with
arbitrary whitespace allowed for visual formatting.  Also comments
beginning with '#' may be used.  Parameters for a particular
observatory begin with the "observatory" parameter and end with the
next observatory definitions (or end-of-file).  For example:

.nf
observatory = "kpno"
	name = "Kitt Peak National Observatory"
	longitude = 111:36.0
	latitude = 31:58.8
	altitude = 2120.
	timezone = 7

observatory = "ctio"
	<etc.>
.fi

String parameters must be quoted if they contain whitespace.
.ih
SEE ALSO
Source code
.endhelp


# Symbol table definitions.
define	LEN_INDEX	10		# Length of symtab index
define	LEN_STAB	512		# Length of symtab
define	SZ_SBUF		512		# Size of symtab string buffer
define	SYMLEN		40		# Length of symbol structure
define	SZ_OBSVAL	79		# Size of observatory value string

# Symbol table structure
define	OBSVAL		Memc[P2C($1)]	# Observatory value string


# OBSOPEN -- Open observatory database and store the requested observatory
# information in symbol table.

pointer procedure obsopen (observatory)

char	observatory[ARB]		# Observatory name
pointer	obsvopen()
errchk	obsvopen

begin
	return (obsvopen (observatory, NO))
end


# OBSVOPEN -- Open observatory database and store the requested observatory
# information in symbol table.

pointer procedure obsvopen (observatory, verbose)

char	observatory[ARB]		# Observatory name
int	verbose				# Verbose?
pointer	obs				# Observatory symbol table pointer

int	fd, envfind(), envgets(), open(), fscan(), nscan(), nowhite()
pointer	sp, fname, obsname, key, str, temp, sym
pointer	stopen(), stenter()
bool	found, streq(), strne()
errchk	open, stopen, stenter, envfind, envgets, fscan

string	obskey	"observatory"
define	getobs_	99

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (obsname, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Open observatory database.
	if (envfind ("obsdb", Memc[fname], SZ_FNAME) <= 0) {
	    call strcpy ("noao$lib/obsdb.dat", Memc[fname], SZ_FNAME)
	    if (verbose == YES) {
		call eprintf ("Using default observatory database: %s\n")
		    call pargstr ("noao$lib/obsdb.dat")
	    }
	} else if (verbose == YES) {
	    call eprintf (
		"Using database defined by '%s' environment variable: %s\n")
		call pargstr ("obsdb")
		call pargstr (Memc[fname])
	}
	fd = open (Memc[fname], READ_ONLY, TEXT_FILE)

	# Set observatory from the environment or task parameter if needed.
	# Convert to lower case but save original name for documentation.

	if (streq (observatory, obskey)) {
	    if (envgets (obskey, Memc[obsname], SZ_FNAME) <= 0) {
		call clgstr ("observatory.observatory", Memc[obsname], SZ_FNAME)
		if (verbose == YES) {
		    call eprintf (
			"Using observatory defined by observatory task: %s\n") 
			call pargstr (Memc[obsname])
		}
	    } else if (verbose == YES) {
		call eprintf (
    		"Using observatory defined by '%s' environment variable: %s\n")
		    call pargstr (observatory)
		    call pargstr (Memc[obsname])
	    }
	} else
	    call strcpy (observatory, Memc[obsname], SZ_LINE)

getobs_ 
	# Strip whitespace and convert to lower case.
	call strcpy (Memc[obsname], Memc[temp], SZ_LINE)
	if (nowhite (Memc[obsname], Memc[obsname], SZ_LINE) > 0)
	    call strlwr (Memc[obsname])

	if (streq (Memc[obsname], "obspars")) {
	    if (verbose == YES) {
		call eprintf (
		    "Using observatory parameters from observatory task\n")
	    }

	    # Create symbol table.
	    obs = stopen (Memc[obsname], LEN_INDEX, LEN_STAB, SZ_SBUF)
	    sym = stenter (obs, obskey, SYMLEN)
	    call strcpy (Memc[obsname], OBSVAL(sym), SZ_OBSVAL)
#	    sym = obspars (obs, "name")
#	    sym = obspars (obs, "longitude")
#	    sym = obspars (obs, "latitude")
#	    sym = obspars (obs, "altitude")
#	    sym = obspars (obs, "timezone")
	} else {
	    if (verbose == YES) {
		call eprintf (
		    "Using observatory parameters for database entry: %s\n")
		    call pargstr (Memc[temp])
	    }

	    # Find observatory entry.
	    found = false
	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[key], SZ_FNAME)
		call gargwrd (Memc[str], SZ_LINE)
		call gargwrd (Memc[str], SZ_LINE)
		if (nscan()<3 || Memc[key]=='#' || strne (Memc[key], obskey))
		    next
		call strlwr (Memc[str])
		if (streq (Memc[str], Memc[obsname])) {
		    found = true
		    break
		}
	    }

	    # Check if entry was found.
	    if (!found) {
		if (Memc[obsname] != EOS && Memc[obsname] != '?') {
		    call eprintf (
		    "WARNING: Observatory entry %s not found in database %s")
			call pargstr (Memc[temp])
			call pargstr (Memc[fname])
		}

		# List database contents and try again
		call seek (fd, BOF)
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[key], SZ_FNAME)
		    call gargwrd (Memc[str], SZ_LINE)
		    call gargwrd (Memc[str], SZ_LINE)
		    if (nscan() < 3 || Memc[key] == '#')
			next
		    if (streq (Memc[key], obskey)) {
			call eprintf ("\n  %s: ")
			    call pargstr (Memc[str])
		    } else if (streq (Memc[key], "name"))
			call eprintf (Memc[str])
		}
		call seek (fd, BOF)
		call eprintf (
		   "\n  obspars:  Use parameters from OBSERVATORY task\n\n")
		call flush (STDERR)
		call clgstr ("observatory.override", Memc[obsname], SZ_LINE)
		goto getobs_
	    }

	    # Create symbol table.
	    obs = stopen (Memc[obsname], LEN_INDEX, LEN_STAB, SZ_SBUF)

	    # Read the file and enter the parameters in the symbol table.
	    sym = stenter (obs, Memc[key], SYMLEN)
	    call strcpy (Memc[obsname], OBSVAL(sym), SZ_OBSVAL)
	    while (fscan(fd) != EOF) {
		call gargwrd (Memc[key], SZ_FNAME)
		call gargwrd (Memc[str], SZ_LINE)
		call gargwrd (Memc[str], SZ_LINE)
		if (nscan() < 3 || Memc[key] == '#')
		    next
		if (streq (Memc[key], obskey))
		    break
		sym = stenter (obs, Memc[key], SYMLEN)
		call strcpy (Memc[str], OBSVAL(sym), SZ_OBSVAL)
	    }
	}

	call close (fd)
	call sfree (sp)

	return (obs)
end


# OBSCLOSE -- Close the observatory symbol table pointer.

procedure obsclose (obs)

pointer	obs			# Observatory symbol table pointer

begin
	if (obs != NULL)
	    call stclose (obs)
end


# OBSPARS -- Get parameter and if not found possibly get it from the
# observatory task

pointer procedure obspars (obs, param)

pointer	obs		# Observatory pointer
char	param[ARB]	# Parameter
pointer	sym		# Symbol table pointer

bool	streq()
pointer	sp, str, stfind(), stenter()
double	clgetd()

begin
	sym = stfind (obs, param)
	if (sym != NULL)
	    return (sym)

	sym = stfind (obs, "observatory")
	if (!streq (OBSVAL(sym), "obspars")) {
	    return (NULL)
	}

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "observatory.%s")
	    call pargstr (param)

	if (streq (param, "name")) {
	    sym = stenter (obs, param, SYMLEN)
	    call clgstr (Memc[str], OBSVAL(sym), SZ_OBSVAL)
	} else if (streq (param, "longitude")) {
	    sym = stenter (obs, param, SYMLEN)
	    call sprintf (OBSVAL(sym), SZ_OBSVAL, "%g")
		call pargd (clgetd (Memc[str]))
	} else if (streq (param, "latitude")) {
	    sym = stenter (obs, param, SYMLEN)
	    call sprintf (OBSVAL(sym), SZ_OBSVAL, "%g")
		call pargd (clgetd (Memc[str]))
	} else if (streq (param, "altitude")) {
	    sym = stenter (obs, param, SYMLEN)
	    call sprintf (OBSVAL(sym), SZ_OBSVAL, "%g")
		call pargd (clgetd (Memc[str]))
	} else if (streq (param, "timezone")) {
	    sym = stenter (obs, param, SYMLEN)
	    call sprintf (OBSVAL(sym), SZ_OBSVAL, "%g")
		call pargd (clgetd (Memc[str]))
	}

	call sfree (sp)
	return (sym)
end
	


# OBSLOG -- Log current observatory

procedure obslog (obs, task, params, fd)

pointer	obs			# Observatory symbol table pointer
char	task[ARB]		# Task name, image name, or other string
char	params[ARB]		# Parameters to log
int	fd			# File descriptor

int	ip, ctowrd()
pointer	sym, obspars()
pointer	sp, param

begin
	call smark (sp)
	call salloc (param, SZ_FNAME, TY_CHAR)

	# Log task string and observatory name
	sym = obspars (obs, "name")
	if (sym == NULL)
	    sym = obspars (obs, "observatory")

	call fprintf (fd, "# ")
	if (task[1] != EOS) {
	    call fprintf (fd, "%s: ")
		call pargstr (task)
	}
	call fprintf (fd,  "Observatory parameters for %s\n")
	    call pargstr (OBSVAL(sym))

	for (ip=1; ctowrd (params, ip, Memc[param], SZ_FNAME) > 0;) {
	    sym = obspars (obs, Memc[param])
	    if (sym == NULL)
		next
	    call fprintf (fd, "#\t%s = %s\n")
		call pargstr (Memc[param])
		call pargstr (OBSVAL(sym))
	}
	call flush (fd)

	call sfree (sp)
end


# OBSGETI -- Get integer observatory parameter.

int procedure obsgeti (obs, param)

pointer	obs			# Observatory symbol table pointer
char	param[ARB]		# Observatory parameter

int	ip, ival, ctoi()
pointer	sym, obspars()
errchk	obspars

begin
	sym = obspars (obs, param)
	if (sym == NULL)
	    call error (1, "OBSGETI: Observatory parameter not found")
	ip = 1
	if (ctoi (OBSVAL(sym), ip, ival) <= 0)
	    call error (1, "OBSGETI: Observatory parameter not integer")
	return (ival)
end


# OBSGETR -- Get real observatory parameter.

real procedure obsgetr (obs, param)

pointer	obs			# Observatory symbol table pointer
char	param[ARB]		# Observatory parameter

int	ip, ctor()
real	rval
pointer	sym, obspars()
errchk	obspars

begin
	sym = obspars (obs, param)
	if (sym == NULL)
	    call error (1, "OBSGETR: Observatory parameter not found")
	ip = 1
	if (ctor (OBSVAL(sym), ip, rval) <= 0)
	    call error (1, "OBSGETR: Observatory parameter not real")
	return (rval)
end


# OBSGETD -- Get double observatory parameter.

double procedure obsgetd (obs, param)

pointer	obs			# Observatory symbol table pointer
char	param[ARB]		# Observatory parameter

int	ip, ctod()
double	dval
pointer	sym, obspars()
errchk	obspars

begin
	sym = obspars (obs, param)
	if (sym == NULL)
	    call error (1, "OBSGETD: Observatory parameter not found")
	ip = 1
	if (ctod (OBSVAL(sym), ip, dval) <= 0)
	    call error (1, "OBSGETD: Observatory parameter not double")
	return (dval)
end


# OBSGSTR -- Get string valued observatory parameter.

procedure obsgstr (obs, param, str, maxchar)

pointer	obs			# Observatory symbol table pointer
char	param[ARB]		# Observatory parameter
char	str[maxchar]		# Observatory parameter value
int	maxchar			# Maximum characters for string

pointer	sym, obspars()
errchk	obspars

begin
	sym = obspars (obs, param)
	if (sym == NULL)
	    call error (1, "OBSGSTR: Observatory parameter not found")
	call strcpy (OBSVAL(sym), str, maxchar)
end


# OBSIMOPEN - Open/reopen observatory for an image.
# Check if the OBSERVAT keyword is found.  If found open/reopen the
# observatory.  If not found open/reopen the default observatory.
# Return flags indicating a change in observatory and whether the
# observatory was defined in the image.

procedure obsimopen (obs, im, observatory, verbose, newobs, obshead)

pointer	obs			#U Observatory symbol table pointer
pointer	im			#I Image pointer
char	observatory[ARB]	#I Default observatory
int	verbose			#I Verbose?
bool	newobs			#O New observatory?
bool	obshead			#O Observatory found in header?	

bool	strne()
pointer	sp, observat, sym, obsvopen(), obspars()
errchk	obsvopen

begin
	call smark (sp)
	call salloc (observat, SZ_FNAME, TY_CHAR)

	if (verbose == YES) {
	    call imstats (im, IM_IMAGENAME, Memc[observat], SZ_FNAME)
	    call eprintf ("%s: ")
		call pargstr (Memc[observat])
	}

	newobs = false
	ifnoerr (call imgstr (im, "observat", Memc[observat], SZ_FNAME)) {
	    if (verbose == YES) {
		call eprintf ("OBSERVAT = %s\n")
		    call pargstr (Memc[observat])
	    }

	    call strlwr (Memc[observat])
	    if (obs == NULL) {
		obs = obsvopen (Memc[observat], verbose)
		newobs = true
	    } else {
		sym = obspars (obs, "observatory")
		if (strne (Memc[observat], OBSVAL(sym))) {
		    call obsclose (obs)
		    obs = obsvopen (Memc[observat], verbose)
		    newobs = true
		}
	    }
	    obshead = true
	} else {
	    if (verbose == YES) {
		call eprintf ("No OBSERVAT keyword - using %s\n")
		    call pargstr (observatory)
	    }

	    if (obs == NULL) {
		obs = obsvopen (observatory, verbose)
		newobs = true
	    } else {
		sym = obspars (obs, "observatory")
		if (strne (observatory, OBSVAL(sym))) {
		    call obsclose (obs)
		    obs = obsvopen (observatory, verbose)
		    newobs = true
		}
	    }
	    obshead = false
	}

	call sfree (sp)
end


# OBSINFO -- List observatory parameters

procedure obsinfo (obs, fd)

pointer	obs			# Observatory symbol table pointer
int	fd			# Output file descriptor

pointer	sym, name, obspars(), sthead(), stnext(), stname()
int	stridxs()
bool	streq()
errchk	obspars
string	obskey	"observatory"

begin
	sym = obspars (obs, obskey)
	call fprintf (fd, "\t%s = %s\n")
	    call pargstr (obskey)
	    call pargstr (OBSVAL(sym))

	if (streq (OBSVAL(sym), "obspars")) {
	    sym = obspars (obs, "name")
	    sym = obspars (obs, "longitude")
	    sym = obspars (obs, "latitude")
	    sym = obspars (obs, "altitude")
	    sym = obspars (obs, "timezone")
	}

	for (sym = sthead (obs); sym != NULL; sym = stnext (obs, sym)) {
	    name = stname (obs, sym)
	    if (streq (Memc[name], obskey))
		next
	    if (stridxs (" 	", OBSVAL(sym)) > 0)
		call fprintf (fd, "\t%s = '%s'\n")
	    else
		call fprintf (fd, "\t%s = %s\n")
		call pargstr (Memc[name])
		call pargstr (OBSVAL(sym))
	}
end
