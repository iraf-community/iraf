# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"help.h"

.help tlist
.nf __________________________________________________________________________
TLIST -- Routines for processing the template list.
Functions:
	tl_open	    	Expand the template list into a list
			    of unambiguous templates.
	tl_read	    	Fetch next template from the list.
	tl_close	Close the list.

Do not confuse these routines with those used to expand and read from the
template for a single package.  Our main function is to pass through the
template list, adding package names where they were left out by the user.
If no package name is given all packages in the database are searched.
.endhelp ______________________________________________________________________

define	SZ_TLSBUF	1024		# local string buffer
define	INC_SZTLSBUF	512		# increment if overflow
define	SZ_CURPACK	31		# allocation for curpack name string

define	LEN_TLSTRUCT	7
define	TL_CTRL		Memi[$1]	# string buffer
define	TL_SBUF		Memi[$1+1]	# string buffer
define	TL_SZSBUF	Memi[$1+2]	# size of string buffer
define	TL_NEXTCH	Memi[$1+3]	# index of next char in sbuf
define	TL_LISTPTR	Memi[$1+4]	# for fetching list elements
define	TL_CURPACK	Memi[$1+5]	# offset of name of current package
define	TL_LISTLEN	Memi[$1+6]	# number of elements in list


# TL_OPEN -- Take the template list supplied by the user and produce as
# output a list wherein each element has an explicit package name.  If an
# element is a module name or a module matching template, it gets replaced
# by a set of such templates, one for each package in the database.
# If a package matching template is given, it is expanded into a list of
# packages.

pointer procedure tl_open (db, hp, tlist, ctrl)

pointer	db			# database descriptor
pointer	hp			# system package directory
char	tlist[ARB]		# template list string
pointer	ctrl

int	ip, junk
pointer	sp, op, tl, sbuf, template, pakstr, modstr
int	tl_fetchelem(), tl_matchpak(), tl_putstr(), tl_getcurpack()
errchk	tl_getcurpack, tl_ambiguous, tl_matchpak
define	paknotfound_	91

begin
	call smark (sp)
	call salloc (template, SZ_LINE, TY_CHAR)
	call salloc (pakstr, SZ_FNAME, TY_CHAR)
	call salloc (modstr, SZ_FNAME, TY_CHAR)

	call calloc (tl, LEN_TLSTRUCT, TY_STRUCT)
	call malloc (sbuf, SZ_TLSBUF, TY_CHAR)

	TL_CTRL(tl)    = ctrl
	TL_SBUF(tl)    = sbuf
	TL_SZSBUF(tl)  = SZ_TLSBUF
	TL_NEXTCH(tl)  = SZ_CURPACK + 2
	TL_LISTPTR(tl) = SZ_CURPACK + 2
	TL_LISTLEN(tl) = 0

	# If null template list, set to "curpack.".  This will cause the
	# help for the current package to be printed.  Otherwise expand
	# the template.  Template expansion can produce a very big list.

	for (ip=1;  IS_WHITE (tlist[ip]);  ip=ip+1)
	    ;

	if (tlist[ip] == EOS) {
	    # Put current package name into list.
	    op = template + tl_getcurpack (tl, Memc[template], SZ_LINE)
	    Memc[op] = '.'
	    Memc[op+1] = EOS
	    junk = tl_putstr (tl, Memc[template])

	} else {
	    # Expand nonnull template list.  Each time through the loop
	    # processes the next comma delimited template.

	    while (tl_fetchelem (tlist,ip,Memc[pakstr],Memc[modstr]) > 0) {
		if (Memc[pakstr] == EOS) {
		    if (H_OPTION(ctrl) == O_DIR) {
			# User wants to print a help directory, but left out
			# the "." in the package name.  Assume the package
			# name is an abbreviation and all all packages which
			# match to the list.

			if (tl_matchpak (tl, hp, Memc[modstr], "") == 0)
			    goto paknotfound_
		    } else
			call tl_ambiguous (tl, db, hp, Memc[modstr])

		} else if (tl_matchpak (tl,hp,Memc[pakstr],Memc[modstr]) == 0) {
		    # Explicit package template was given by user, but no
		    # matches were made to packages installed in the system
		    # help directory.  Inform user.
paknotfound_
		    call eprintf ("Cannot find help for package `%s'\n")
			call pargstr (Memc[pakstr])
		}
	    }
	}

	# Return unused space in string buffer.  Copy length of list to
	# the main help structure.

	call realloc (TL_SBUF(tl), TL_NEXTCH(tl), TY_CHAR)
	TL_SZSBUF(tl) = TL_NEXTCH(tl)
	H_LENTL(ctrl) = TL_LISTLEN(tl)

	call sfree (sp)
	return (tl)
end


# TL_CLOSE -- Close a template list opened with TL_OPEN.

procedure tl_close (tl)

pointer	tl

begin
	call mfree (TL_SBUF(tl), TY_CHAR)
	call mfree (tl, TY_STRUCT)
end


# TL_READ -- Get the next template from the template list.
# Returns EOF when end of list is reached, else nchars in template.

int procedure tl_read (tl, outstr, maxch)

pointer	tl
char	outstr[ARB]
int	maxch
int	offset, nchars
int	gstrcpy()

begin
	if (TL_LISTPTR(tl) >= TL_NEXTCH(tl))
	    return (EOF)

	offset = TL_LISTPTR(tl)
	nchars = gstrcpy (Memc[TL_SBUF(tl)+offset], outstr, maxch)
	TL_LISTPTR(tl) = offset + nchars + 1

	return (nchars)
end


# TL_MATCHPACK -- Match package name against the system package list,
# add each package matched to the list.

int procedure tl_matchpak (tl, hp, package, module)

pointer	tl
pointer	hp
char	package[ARB]
char	module[ARB]

int	paklen, junk, ngen, modnum
pointer	sp, template, op, ml
int	tl_putstr(), ml_read()
pointer	ml_open()
errchk	ml_open, ml_read, tl_putstr

begin
	call smark (sp)
	call salloc (template, SZ_LINE, TY_CHAR)

	ngen = 0
	ml = ml_open (hp, package)

	repeat {
	    paklen = ml_read (ml, modnum, Memc[template], SZ_LINE)
	    if (paklen == EOF)
		break
	    op = template + paklen
	    Memc[op] = '.'
	    call strcpy (module, Memc[op+1], SZ_LINE-paklen-1)
	    junk = tl_putstr (tl, Memc[template])
	    ngen = ngen + 1
	}

	call ml_close (ml)
	call sfree (sp)
	return (ngen)
end


# TL_AMBIGUOUS -- Called to expand an ambiguous template, i.e., one which did
# not include the "." delimiter to denote the package and module fields.
# We handle this by generating a series of module matching templates, one for
# each package in the database, starting with the current package.  If the
# ambiguous pattern contains any pattern matching metacharacters we match only
# against the modules in the current package.  Otherwise (completely ambiguous
# template) we match against:
#
#	[1] all modules in the current package
#	[2] all modules in all packages, except the current package
# 
# For example, "alpha" might be expanded into the list
#	curpack.alpha, pak1.alpha, pak2.alpha, ...
#
# The search [2] is a depth-first search of all packages in the root.  Since
# each non-root package is a module of some other package, this search will
# find all non-root packages as well as modules.  For the root packages to be
# found they must reference themselves as modules, but NOT as subpackages,
# otherwise recursion will occur.

procedure tl_ambiguous (tl, db, hp, module)

pointer	tl			# template list descriptor
pointer	db			# database descriptor
pointer	hp			# package directory
char	module[ARB]		# module template

int	paklen, junk, ip, pk
pointer	sp, curpack, template, op
bool	streq()
int	tl_putstr(), stridx(), tl_getcurpack(), hd_getname()
errchk	tl_putstr, tl_getcurpack, hd_getname

begin
	call smark (sp)
	call salloc (template, SZ_LINE, TY_CHAR)
	call salloc (curpack, SZ_FNAME, TY_CHAR)

	# Output the template for the current package.  Save the name of
	# the current package for later use.

	op = template + tl_getcurpack (tl, Memc[template], SZ_LINE)
	call strcpy (Memc[template], Memc[curpack], SZ_FNAME)
	Memc[op] = '.'
	call strcpy (module, Memc[op+1], SZ_LINE-(op-template)-1)
	junk = tl_putstr (tl, Memc[template])

	# Check for pattern matching metacharacters.  If present we match
	# only against the current package, hence we are all done.

	for (ip=1;  module[ip] != EOS;  ip=ip+1)
	    if (stridx (module[ip], "*?[]") > 0) {
		call sfree (sp)
		return
	    }

	# Output a template for each remaining package in the database,
	# excluding the current package which gets searched first and has
	# already been put in the list.  Template is "pakname.module".

	pk = 1
	repeat {
	    paklen = hd_getname (hp, pk, TY_MODNAME, Memc[template], SZ_LINE)
	    pk = pk + 1
	    if (paklen <= 0)
		break
	    if (streq (Memc[template], Memc[curpack]))
		next
	    op = template + paklen
	    Memc[op] = '.'
	    call strcpy (module, Memc[op+1], SZ_LINE-paklen-1)
	    junk = tl_putstr (tl, Memc[template])
	}

	call sfree (sp)
end


# TL_FETCHELEM -- Extract next element from template list.  An element
# consists of the package pattern, module pattern, both, or either.
# Set output strings to the null string if field not present.  Return
# the number of chars in the full element, zero when EOS is reached on
# the template list.

int procedure tl_fetchelem (tlist, ip, pakstr, modstr)

char	tlist[ARB]
int	ip
char	pakstr[SZ_FNAME]
char	modstr[SZ_FNAME]

bool	package_seen
char	ch
int	op, ip_save

begin
	package_seen = false
	pakstr[1] = EOS

	# Skip whitespace, the comma delimiters between list elements,
	# and any null list elements (i.e. ",,").

	while (IS_WHITE (tlist[ip]) || tlist[ip] == ',')
	    ip = ip + 1
	ip_save = ip

	# Extract the first field.  If period is seen, this is the package
	# field, copy it to pakstr.  Remainder is the module name.
	op = 1
	ch = tlist[ip]
	while (ch != ',' && ch != '\n' && ch != EOS) {
	    modstr[op] = ch
	    if (ch == '.') {
		if (package_seen)
		    call error (18, "Too many '.' delims in help template")
		package_seen = true
		modstr[op] = EOS
		call strcpy (modstr, pakstr, SZ_FNAME)
		op = 1
	    } else
		op = op + 1
	    ip = ip + 1
	    ch = tlist[ip]
	}
	modstr[op] = EOS

	return (ip - ip_save)
end


# TL_GETCURPACK -- Get the name of the current package.  The hidden CL command
# "_curpack" prints the name of the current package.

int procedure tl_getcurpack (tl, pakname, maxch)

pointer	tl			# template list
char	pakname[maxch]		# package name (output)
int	maxch

pointer	ctrl
int	nchars, junk
int	gstrcpy(), fscan()
bool	strne()
errchk	clcmd

begin
	# The current package is read only once for each tlist expansion.
	# The following uses the CL global parameter "list" to read the
	# package list.  The CL interface is violated by sending an explicit
	# command to the CL.  Poor practice, but the query can be disabled
	# by setting the help.curpack parameter, if this causes a problem.

	if (TL_CURPACK(tl) == NULL) {
	    # Was the current package set via a help task parameter?
	    ctrl = TL_CTRL(tl)
	    if (H_CURPACK(ctrl) != EOS)
		if (strne (H_CURPACK(ctrl), "AskCL")) {
		    call strcpy (H_CURPACK(ctrl),
			Memc[TL_SBUF(tl)+1], SZ_CURPACK)
		    TL_CURPACK(tl) = 1
		}

	    if (TL_CURPACK(tl) == NULL) {
		# Send "_curpack" command to the CL, read the response (a single
		# line) from CLIN.

		call clcmd ("_curpack")
		junk = fscan (CLIN)
		    call gargwrd (Memc[TL_SBUF(tl)+1], SZ_CURPACK)
		TL_CURPACK(tl) = 1
	    }
	}

	nchars = gstrcpy (Memc[TL_SBUF(tl) + TL_CURPACK(tl)], pakname, maxch)
	return (nchars)
end


# TL_PUTSTR -- Put a string (incl EOS) in the string buffer at nextch.
# If there is not enough space in the buffer, reallocate a larger buffer.
# Return the index of the string in the string buffer.

int procedure tl_putstr (tl, str)

pointer	tl
char	str[ARB]
int	nextch, nchars, strlen()
errchk	realloc

begin
	# Null strings are not stored and cause a null index to be returned.
	nchars = strlen (str)
	if (nchars == 0)
	    return (0)

	nextch = TL_NEXTCH(tl)
	if (nextch + nchars + 1 > TL_SZSBUF(tl)) {
	    TL_SZSBUF(tl) = TL_SZSBUF(tl) + INC_SZTLSBUF
	    call realloc (TL_SBUF(tl), TL_SZSBUF(tl), TY_CHAR)
	}

	call strcpy (str, Memc[TL_SBUF(tl) + nextch], ARB)
	TL_NEXTCH(tl) = nextch + nchars + 1
	TL_LISTLEN(tl) = TL_LISTLEN(tl) + 1

	return (nextch)
end
