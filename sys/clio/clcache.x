# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<clio.h>

.help clcache
.nf ___________________________________________________________________________
CLCACHE -- A package for cacheing the values of static parameters, i.e., 
parameters with values fixed at task invocation time.

The purpose of this package is to improve the runtime performance of the
parameter passing mechanism.  The runtime semantics of the CLIO interface are
not affected.  Transmission of the static parameters during task invocation
can save many runtime context switches, saving seconds of clock time when
running tasks which have many (dozens of) parameters.


		 clc_init ()			# initialize the cache
	     clc_compress ()			# rebuild the cache
	      clc_newtask (taskname)		# set name of root pset
		 clc_mark (sp)			# mark cache status
		 clc_free (sp)			# free to last mark

		clc_enter (param, value)	# cache a parameter
       nchars = clc_fetch (param, out, maxch)	# fetch cached parameter
          symp = clc_find (param, out, maxch)	# find cached parameter

		 clc_scan (cmd)			# scan a param=value stmt
		 clc_list (fd, pset, format)	# list params to a file

The cache is initialized by the IRAF main with CLC_INIT during process
startup and whenever a new task is run.  Parameter value pairs are entered
into the cache with CLC_ENTER during processing of the command line.
Runtime get parameter requests from the task are satisfied from the cache if
possible, querying the CL only if the cached value cannot be found.  Note
that query mode and list type parameters are never cached since they do not
have static values.

A task can be called either with named parameters or with unnamed, positional
parameters.  In the latter case the parameters are passed as "$1", "$2", etc.
If we receive one or more numbered parameters they will be entered into the
symbol table in the usual way but a list of offsets of the positional
arguments will be saved in the clio common.  Subsequent runtime parameter
requests will be satisfied by trying to find the parameter by name in the
symbol table, returning the next positional argument if the named parameter
cannot be found.  This is the mechanism used by the CL to satisfy requests
for parameters from a task which has no parameter file.

The values of all parameters are saved in the cache in string format.  Since
all parameters come from the CL in string format this makes for an easy
interface to the high level CLIO code.  The internal storage format for the
cache is a SYMTAB hash table, simplifying the implementation and providing
optimal performance.  There is no fixed limit on the size of the cache.
.endhelp _____________________________________________________________________

# SYMTAB default allocation parameters (non-limiting).
define	LEN_INDEX	128		# nbuckets in symtab hash index
define	LEN_STAB	512		# initial symbol table size
define	SZ_SBUF		2048		# initial string buffer size


# Symbol table structure (not much to it).

define	LEN_SYMSTRUCT	1
define	SYM_VALUE	Memi[$1]	# sbuf offset of value string


# CLC_INIT -- Initialize the parameter cache.  Called during process
# startup.  May be called repeatedly to reinitialize the cache.

procedure clc_init()

pointer	stopen()
bool	first_time
data	first_time /true/
include	"clio.com"
errchk	stopen

begin
	if (first_time) {
	    cl_stp = stopen ("clcache", LEN_INDEX, LEN_STAB, SZ_SBUF)
	    first_time = false
	} else
	    call stfree (cl_stp, cl_stmark)

	call stmark (cl_stp, cl_stmark)
	call aclri (cl_posarg, MAX_POSARGS)
	cl_nposargs = 0
	cl_nextarg  = 1
end


# CLC_NEWTASK -- Set the name of the task whose parameters are to be
# entered into the cache (the taskname is the root pset).

procedure clc_newtask (taskname)

char	taskname[ARB]		# name of the task being run

int	gstrcpy()
include	"clio.com"

begin
	cl_psetop = gstrcpy (taskname, cl_psetname, SZ_PSETNAMEBUF) + 2
	cl_psetindex[1] = 1
	cl_npsets = 1
end


# CLC_MARK -- Mark storage in the cache for subsequent restoration by
# clc_free.

procedure clc_mark (marker)

pointer	marker			# receives marked position
include	"clio.com"

begin
	call stmark (cl_stp, marker)
end


# CLC_FREE -- Free storage in the cache back to the marked position.  Any
# positional arguments are lost.

procedure clc_free (marker)

pointer	marker			# marked position
include	"clio.com"

begin
	call stfree (cl_stp, marker)
	cl_nposargs = 0
	cl_nextarg  = 1
	call aclri (cl_posarg, MAX_POSARGS)
end


# CLC_ENTER -- Enter a parameter-value pair into the cache.  If the parameter
# is an unnamed positional parameter ($N) it is entered in the usual way
# with name $N, but its symtab pointer is also saved in the positional argument
# list.  It is safe to save the pointer rather than the index because tasks
# which do not have pfiles never have more than a few arguments, hence the
# symtab will not be reallocated during entry.
#
# If the parameter name is of the form psetname.paramname, extract the pset
# name and add it to the list of pset names for the task.  The order in which
# the pset names are defined will be the order in which they are later searched
# when satifying ambiguous references (where the psetname is not specified).

procedure clc_enter (param, value)

char	param[ARB]		# parameter name
char	value[ARB]		# parameter value string

pointer	sym
int	off, ch, pp, op, ip, n
bool	streq()
pointer	stenter()
int	stpstr(), ctoi()
errchk	stenter, syserrs
include	"clio.com"

begin
	sym = stenter (cl_stp, param, LEN_SYMSTRUCT)
	SYM_VALUE(sym) = stpstr (cl_stp, value, 0)

	if (param[1] == '$') {
	    # Positional argument (no pfile/pset).

	    ip = 2
	    if (ctoi (param, ip, n) > 0) {
		n = max(1, min(MAX_POSARGS, n))
		cl_posarg[n] = sym
		cl_nposargs  = max (cl_nposargs, n)
	    }

	} else {
	    # Check if the parameter name includes the psetname prefix,
	    # and if so, append the pset name to the pset name list if
	    # not already there.

	    pp = cl_psetop
	    op = pp

	    # Extract psetname.
	    do ip = 1, SZ_PNAME {
		ch = param[ip]
		if (ch == EOS) {
		    return			# no psetname given
		} else if (ch == '.') {
		    cl_psetname[op] = EOS
		    break
		} else {
		    cl_psetname[op] = ch
		    op = op + 1
		}
	    }

	    # If pset already in list we are done.
	    ch = param[1]
	    do ip = cl_npsets, 1, -1 {
		off = cl_psetindex[ip]
		if (cl_psetname[off] == ch)
		    if (streq (cl_psetname[pp], cl_psetname[off]))
			return
	    }

	    # Pset not found, so enter new pset name into list.
	    cl_npsets = cl_npsets + 1
	    if (cl_npsets > MAX_PSETS)
		call syserrs (SYS_CLNPSETS, cl_psetname[pp])

	    cl_psetindex[cl_npsets] = pp
	    cl_psetop = op + 1
	    if (cl_psetop > SZ_PSETNAMEBUF)
		call syserrs (SYS_CLPSETOOS, cl_psetname[pp])
	}
end


# CLC_FETCH -- Search the CL parameter cache for the named parameter and
# return its value if found.  If the parameter is not found and there are
# positional arguments, return the value of the next positional argument.
# The number of characters in the output string is returned as the function
# value if the parameter is found, else ERR is returned.

int procedure clc_fetch (param, outstr, maxch)

char	param[ARB]		# parameter to be fetched
char	outstr[maxch]		# receives value string of parameter
int	maxch

pointer	sym, vp
int	gstrcpy()
pointer	strefsbuf(), clc_find()
include	"clio.com"

begin
	# Search the symbol table for the named parameter.
	sym = clc_find (param, outstr, maxch)

	# If the named parameter could not be found using the given name or
	# in any pset in the table, use the next positional argument if there
	# is one.

	while (sym == NULL)
	    if (cl_nextarg <= cl_nposargs) {
		sym = cl_posarg[cl_nextarg]
		cl_nextarg = cl_nextarg + 1
	    } else {
		outstr[1] = EOS
		return (ERR)
	    }

	vp = strefsbuf (cl_stp, SYM_VALUE(sym))
	return (gstrcpy (Memc[vp], outstr, maxch))
end


# CLC_FIND -- Search the CL parameter cache for the named parameter and
# return its symtab pointer and full name if found. 

pointer procedure clc_find (param, outstr, maxch)

char	param[ARB]		# parameter to be fetched
char	outstr[maxch]		# receives full name of parameter
int	maxch

pointer	sym
int	op, ip, ch, i
pointer	stfind()
include	"clio.com"

begin
	# Look first for the named parameter, and if that is not found,
	# search each pset for the named parameter, i.e., prepend the name
	# of each pset to produce a name of the form "pset.param", and
	# look that up in the symbol table.  The first entry in the pset
	# name list is the name of the task itself.

	sym = stfind (cl_stp, param)
	if (sym == NULL) {
	    do i = 1, cl_npsets {
		op = 1

		# Start with pset name.
		do ip = cl_psetindex[i], SZ_PSETNAMEBUF {
		    ch = cl_psetname[ip]
		    if (ch == EOS)
			break
		    else {
			cl_pname[op] = ch
			op = op + 1
		    }
		}

		# Add dot delimiter.
		cl_pname[op] = '.'
		op = op + 1

		# Lastly add the parameter name.
		do ip = 1, SZ_FNAME {
		    ch = param[ip]
		    if (ch == EOS)
			break
		    else {
			cl_pname[op] = ch
			op = op + 1
		    }
		}

		# Look it up in the symbol table.
		cl_pname[op] = EOS
		sym = stfind (cl_stp, cl_pname)
		if (sym != NULL)
		    break
	    }
	} else
	    call strcpy (param, cl_pname, SZ_FNAME)

	if (sym != NULL)
	    call strcpy (cl_pname, outstr, maxch)

	return (sym)
end


# CLC_SCAN -- Extract the param and value substrings from a param=value
# statement and enter them into the CL parameter cache.

procedure clc_scan (cmd)

char	cmd[ARB]		#I command to be scanned

int	ip
pointer	sp, param, value, op, nchars
int	stridx(), ctowrd()

begin
	call smark (sp)
	call salloc (param, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_COMMAND, TY_CHAR)

	# Skip any leading whitespace.
	for (ip=1;  IS_WHITE(cmd[ip]);  ip=ip+1)
	    ;

	# Do nothing if blank line or comment.
	if (cmd[ip] == EOS || cmd[ip] == '\n' || cmd[ip] == '#') {
	    call sfree (sp)
	    return
	}

	# Extract the param field.
	op = param
	while (IS_ALNUM (cmd[ip]) || stridx (cmd[ip], "_.$") > 0) {
	    Memc[op] = cmd[ip]
	    op = op + 1
	    ip = ip + 1
	}
	Memc[op] = EOS

	# Advance past the assignment operator.
	while (IS_WHITE (cmd[ip]) || cmd[ip] == '=')
	    ip = ip + 1

	# Get the value string.
	nchars = ctowrd (cmd, ip, Memc[value], SZ_COMMAND)

	# Enter the param=value pair into the CL parameter cache.
	call clc_enter (Memc[param], Memc[value])

	call sfree (sp)
end


# CLC_LIST -- List the parameters in the named pset to an output file using
# a caller supplied format.  If no pset is specified the entire contents of
# the parameter cache are output.  A sample format is "set %s = \"%s\"\n".

procedure clc_list (fd, pset, format)

int	fd			#I output file
char	pset[ARB]		#I pset to be listed, or EOS for full cache
char	format[ARB]		#I output format - one %s each for param,value

int	nsyms, i
pointer	sp, syms, sympset, ip, op, sym, np

bool	strne()
pointer	sthead(), stnext(), stname(), strefsbuf()
include	"clio.com"

begin
	# Count the number of parameters.
	nsyms = 0
	for (sym=sthead(cl_stp);  sym != NULL;  sym=stnext(cl_stp,sym))
	    nsyms = nsyms + 1

	call smark (sp)
	call salloc (syms, nsyms, TY_POINTER)
	call salloc (sympset, SZ_FNAME, TY_CHAR)

	# Get a reversed list of symbol pointers.
	op = syms + nsyms - 1
	for (sym=sthead(cl_stp);  sym != NULL;  sym=stnext(cl_stp,sym)) {
	    Memi[op] = sym
	    op = op - 1
	}

	# Output the list.
	do i = 1, nsyms {
	    sym = Memi[syms+i-1]
	    np = stname (cl_stp, sym)

	    # Check the pset name if the user named a specific pset.
	    if (pset[1] != EOS) {
		# Get the pset name of the parameter.
		op = sympset
		for (ip=np;  Memc[ip] != EOS && Memc[ip] != '.';  ip=ip+1) {
		    Memc[op] = Memc[ip]
		    op = op + 1
		}
		Memc[op] = EOS

		# Skip if the wrong pset.
		if (strne (Memc[sympset], pset))
		    next
	    }

	    call fprintf (fd, format)
		call pargstr (Memc[np])
		call pargstr (Memc[strefsbuf(cl_stp,SYM_VALUE(sym))])
	}

	call sfree (sp)
end


# CLC_COMPRESS -- Compress the parameter cache.  Since every parameter
# modification results in a new parameter entry (redef), the symbol table
# can grow quite large if there are many clput type parameter accesses.
# This operator rebuilds the parameter cache eliminating all old entries.

procedure clc_compress ()

pointer	n_st, o_st
pointer	sym, newsym, np, vp

int	stpstr()
pointer	strefsbuf(), stopen(), stname()
pointer	sthead(), stnext(), stfind(), stenter()
errchk	stopen, stenter, stpstr
include	"clio.com"

begin
	n_st = stopen ("clcache", LEN_INDEX, LEN_STAB, SZ_SBUF)
	o_st = cl_stp

	# Copy the symbol table, saving only the most recent entry for
	# each symbol.

	for (sym=sthead(o_st);  sym != NULL;  sym=stnext(o_st,sym)) {
	    np = stname (o_st, sym)
	    if (stfind (n_st, Memc[np]) == NULL) {
		vp = strefsbuf (o_st, SYM_VALUE(sym))
		newsym = stenter (n_st, Memc[np], LEN_SYMSTRUCT)
		SYM_VALUE(newsym) = stpstr (n_st, Memc[vp], 0)
	    }
	}

	# Copy back the saved symbols.  The "push/pop" way in which we use
	# the temporary symbol table to save the symbols automatically
	# preserves the original symbol table ordering.

	call stfree (o_st, cl_stmark)
	call stmark (o_st, cl_stmark)

	for (sym=sthead(n_st);  sym != NULL;  sym=stnext(n_st,sym)) {
	    np = stname (n_st, sym)
	    vp = strefsbuf (n_st, SYM_VALUE(sym))
	    newsym = stenter (o_st, Memc[np], LEN_SYMSTRUCT)
	    SYM_VALUE(newsym) = stpstr (o_st, Memc[vp], 0)
	}

	call stclose (n_st)
	call stsqueeze (o_st)
end
