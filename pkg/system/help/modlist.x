# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<error.h>
include	"help.h"
include	"helpdir.h"

.help modlist
.nf ___________________________________________________________________________
MODLIST -- Routines for expanding a template into a list of module names,
and accessing successive elements of the list, given a template and a help
directory defining the pattern matching domain as input.

The permissible complexity of a help template is somewhat less than that of
a file template; we do not have to deal with logical directories, virtual
file names, and list files.  The class of characters permitted in a CL
module name is fairly limited.  By the time we are called, the template
list will already have been expanded into a list of templates, wherein
each list element has a full package name (the package field has already
been expanded).  Our job is to process a single template.

	*			all modules
	im*			all modules whose names start with 'im'
	[a-z]?*			all modules beg. with lowercase letter 
	alpha			all modules alpha*

Note that a simple alphanumeric name like 'alpha' is treated as an
abbreviation, i.e., as 'alpha?*'.  All occurrences of the closure metacharacter
* are converted to ?*, except after a character class.

Entry points in this package include:

	ml_open		set up to match and extract module names
	ml_read		get next module name which matches pattern
	ml_close	called when done to return buffers
.endhelp ______________________________________________________________________

define	LEN_MLSTRUCT	165		# module list structure
define	SZ_PATBUF	159

define	ML_HP		Memi[$1]	# help directory
define	ML_INDEX	Memi[$1+1]	# index of next module in helpdir
define	ML_SIMPAT	Memi[$1+2]	# pattern is simple (no metacharacters)
define	ML_PATBUF	Memc[P2C($1+5)] # encoded pattern buffer


# ML_OPEN -- Set up to read module names from a directory which match
# a certain pattern.  Allocate descriptor, save pointer to directory,
# encode pattern, rewind module index.

pointer procedure ml_open (hp, template)

pointer	hp				# help directory
char	template[ARB]			# "alpha", "*", etc.

bool	metachars
int	ip
pointer	ml, sp, pat, op
int	patmake()

begin
	call smark (sp)
	call salloc (pat, SZ_LINE, TY_CHAR)

	iferr (call calloc (ml, LEN_MLSTRUCT, TY_STRUCT))
	    call erract (EA_FATAL)
	ML_INDEX(ml) = 1
	ML_HP(ml) = hp

	# Match only at beginning of module name.  Remember to scrap this
	# metacharacter if matching a SIMPAT simple pattern.

	Memc[pat] = '^'		
	op = pat + 1

	# If no metacharacters present in pattern set SIMPAT flag so that we
	# can use a more efficient pattern matching algorithm.  If closure
	# metacharacter is present and it does not follow ], replace it with ?*.

	metachars = false
	for (ip=1;  template[ip] != EOS && op-pat < SZ_LINE;  ip=ip+1) {
	    switch (template[ip]) {
	    case '?', '[', ']':
		metachars = true
	    case '*':
		metachars = true
		if (ip == 1 || (ip > 1 && template[ip-1] != ']')) {
		    Memc[op] = '?'
		    op = op + 1
		}
	    }
	    Memc[op] = template[ip]
	    op = op + 1
	}
	Memc[op] = EOS

	if (!metachars) {
	    ML_SIMPAT(ml) = YES
	    # Copy pattern string, stripping off the leading '^'.
	    call strcpy (Memc[pat+1], ML_PATBUF(ml), SZ_PATBUF)
	} else {
	    ML_SIMPAT(ml) = NO
	    if (patmake (Memc[pat], ML_PATBUF(ml), SZ_PATBUF) == ERR)
		call error (16, "Cannot encode help pattern")
	}

	call sfree (sp)
	return (ml)
end


# ML_READ -- Get next module name from help directory matching the
# encoded pattern.  Return EOF when directory is exhausted.  We directly
# access the HD structure for efficiency.

int procedure ml_read (ml, modnum, module, maxch)

pointer	ml			# pointer to module list descriptor
int	modnum			# module number (output)
char	module[ARB]		# module name (output)
int	maxch

bool	match_found
char	first_char
int	m
pointer	hp, sbuf, mp, modname
int	patmatch(), ml_patmatch(), gstrcpy()
errchk	patmatch

begin
	if (ml == NULL)
	    call error (17, "help.ml_read called with null descriptor")
	hp = ML_HP(ml)
	sbuf = HD_SBUF(hp)
	first_char = ML_PATBUF(ml)
	m = ML_INDEX(ml)
	match_found = false

	# Scan down the list of modules in the package, returning the name
	# of each matching the pattern in successive calls.  Return EOF when
	# the list is exhausted.

	while (m <= HD_NMODULES(hp)) {
	    mp = HD_MODULE(hp, m)
	    modname = sbuf + M_NAME(mp)
	    m = m + 1
	    if (ML_SIMPAT(ml) == YES) {
		if (Memc[modname] == first_char)
		    if (ml_patmatch (Memc[modname], ML_PATBUF(ml)) > 0) {
			match_found = true
			break
		    }
	    } else if (patmatch (Memc[modname], ML_PATBUF(ml)) > 0) {
		match_found = true
		break
	    }
	}

	ML_INDEX(ml) = m
	modnum = m - 1

	# If no modules were found matching the template then we can only be
	# at the end of the list.

	if (match_found)
	    return (gstrcpy (Memc[modname], module, maxch))
	else {
	    module[1] = EOS
	    return (EOF)
	}
end


# ML_PATMATCH -- Determine if a SIMPAT simple pattern is an abbreviation of
# a module name.  The pattern is simple, i.e., contains no metacharacters.

int procedure ml_patmatch (module, pattern)

char	module[ARB]		# module name to be compared to pattern
char	pattern[ARB]		# pattern of the form "^abbrev"
int	ip

begin
	for (ip=1;  pattern[ip] != EOS;  ip=ip+1)
	    if (pattern[ip] != module[ip])
		return (0)

	return (1)
end


# ML_CLOSE -- Return buffers allocated by open_template.

procedure ml_close (ml)

pointer	ml

begin
	call mfree (ml, TY_STRUCT)
end
